;;-----------------------------------------------------------------
(define sc-port 57110)
(define sc-possible-paths
  (list
   "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth" ; regular mac
   "/Applications/SuperCollider.app/Contents/Resources/scsynth")) ; homebrew mac

;;-----------------------------------------------------------------
;; Open a UDP connection to SuperCollider (SC3)
(define sc3 (so/udp:open "127.0.0.1" sc-port))

(define (sc3-responds?)
  (guard (x [else #f])
    (sc/server-sample-rate-nominal sc3) #t))

;; If there's an SC3 instance running, use it, otherwise launch one.
(if (sc3-responds?)
    (printfln "Reusing SuperCollider process at port: ~a" sc-port)
    (begin (printfln "Launching SuperCollider process at port: ~a" sc-port)
           (launch-supercollider sc-port sc-possible-paths)
           (sleep-secs 0.2)))

;; Reset to a blank server with default (root) group 1
;; TODO: Rude if someone has a running session...
;; Can we instead only reset groups created by a previous
;; aeon session? How would we do that?
(if (sc3-responds?)
    (begin (sc/reset sc3)
           (sleep-secs 0.2))
    (error "init" "\nUnable to launch SuperCollider."))

;;-----------------------------------------------------------------
;; Get the time in a format that SC3 understands
(define* (utc [/opt (offset-secs 0)])
  (+ (sc/utc) offset-secs))

;; Send a timestamped OSC bundle to SC3.
(define (send-bundle t args)
  (so/send sc3 (so/bundle t args)))

;;-----------------------------------------------------------------
;; Groups are used to organise our synths on the SC3 server.
(define standard-group    (make-unused-group-id)) ;; voices with no :group go here
(define send-effect-group (make-unused-group-id)) ;; send fx go here
(define recording-group   (make-unused-group-id)) ;; recording synths go here

;; We keep a list of all other groups that get created so we
;; don't spam new group commands to SC. After init, these fns
;; and data are accessed only from the playback thread.
(define known-groups (list))
(define (known-group? id) (member id known-groups))

(define (register-group id)
  (let ([needs-adding (and (number? id) (> id 1) (not (known-group? id)))])
    (when needs-adding
      (set! known-groups (cons id known-groups)))
    needs-adding))

;; Create a generic group with no special routing.
(define (create-group id target order)
  (when (register-group id)
    (so/send sc3 (sc/g-new1 id order target))))

;; A 'voice group' contains N voices, then N effects, then
;; one special synth to route the output to master.
;; Each voice group has its own audio bus. Voices write
;; to this using Out, effects use ReplaceOut.
(define (voice-group-out-id group-id) (+ 32768 group-id))
(define (voice-group-bus group-id) (+ 256 group-id))

(send-synth sc3 "voice-group-out-synth"
  (letc ([:in 0 ir])
    (mrg2 (out 0 (stereo-in :in))
          (replace-out :in (silence 2)))))

(define* (create-voice-group id [/opt (t (utc 0.1))])
  (when (register-group id)
    (let ([out-id (voice-group-out-id id)])
      (send-bundle t (list (sc/g-new1 id sc/add-to-head root-group)
                           (sc/s-new0 "voice-group-out-synth" out-id sc/add-to-tail id)
                           (sc/n-set out-id (make-alist ":in" (voice-group-bus id))))))))

;; Finally we create the basic groups.
;; Other groups will be created as needed.
(create-voice-group standard-group)
(create-group send-effect-group root-group sc/add-to-tail)
(create-group recording-group root-group sc/add-to-tail)
(sleep-secs 0.125)

;;-----------------------------------------------------------------
;; Send a timestamped OSC bundle to launch a new synth with some arguments.
;; Usually there's no need to specify a synth-id unless you know what you're doing.
(define* (play-when name t arg-pairs
                    [/opt (target standard-group)
                          (action sc/add-to-head)
                          (synth-id -1)])
  (send-bundle t (list (sc/s-new0 name synth-id action target)
                       (sc/n-set synth-id arg-pairs))))

;; Sends a control change to all the voices in group.
(define (control-when t arg-pairs group)
  (send-bundle t (list (sc/n-set group arg-pairs))))

;; Add an effect to all the voices in group.
(define (fx-when name t arg-pairs group)
  (let ([out-synth-id (voice-group-out-id group)])
    (play-when name t arg-pairs out-synth-id sc/add-before)))

;; Useful for quick testing of synthdefs
(define (play-now name arg-pairs)
  (play-when name (utc) arg-pairs))

;; Much like play-when, but adds to tail
;; Avoid confusing 'late' messages by delaying 0.1
(define (start-send-effect name . arg-pairs)
  (play-when name (utc 0.1) arg-pairs
             send-effect-group sc/add-to-tail))

;; Kills a running synth immediately (may cause clicks)
(define* (stop-synth synth-id [/opt (t (sc/utc))])
  (send-bundle t (list (sc/n-free1 synth-id))))
