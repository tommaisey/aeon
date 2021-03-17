;;-----------------------------------------------------------------
(define sc-port 57110)
(define sc-possible-paths
  (list
   "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth" ; regular mac
   "/Applications/SuperCollider.app/Contents/Resources/scsynth")) ; homebrew ma

(launch-supercollider sc-port sc-possible-paths)

;;-----------------------------------------------------------------
;; Open the UDP connection to SuperCollider
(define sc3 (so/udp:open "127.0.0.1" sc-port))

;; Send a timestamped OSC bundle.
(define (send-bundle t args)
  (so/send sc3 (so/bundle t args)))

;;-----------------------------------------------------------------
;; We keep a list of all the groups that have been created so
;; we don't spam new group commands to SC. After init, these fns
;; and data are accessed only from the playback thread.
(define default-group 1) ;; See SC docs on default_group
(define group-id-counter default-group)

(define (make-unused-group-id)
  (set! group-id-counter (+ group-id-counter 1))
  group-id-counter)

(define known-groups (list))
(define (known-group? id) (member id known-groups))

(define (register-group id)
  (let ([needs-adding (and (number? id) (> id 1) (not (known-group? id)))])
    (when needs-adding
      (set! known-groups (cons id known-groups)))
    needs-adding))

;; Create a group with no special routing (e.g. for send fx, recording)
(define (create-group id order target)
  (when (register-group id)
    (so/send sc3 (sc/g-new1 id order target))))

;; Groups containing voices and fx are each given their own bus.
;; Voices write to this bus using Out, while fx use ReplaceOut.
;; A special synth at the end of the group routes the result to main out.
(define voice-group-out-synth "voice-group-out-synth")
(define (voice-group-out-id group-id) (+ 32768 group-id))
(define (voice-group-bus group-id) (+ 256 group-id))

(define* (create-voice-group id [/opt (t (sc/utc))])
  (when (register-group id)
    (let ([out-id (voice-group-out-id id)])
      (send-bundle t
        (list (sc/g-new1 id sc/add-to-head default-group)
              (sc/s-new0 voice-group-out-synth out-id sc/add-to-tail id)
              (sc/n-set out-id (make-alist ":in" (voice-group-bus id))))))))

;; n.b. SC docs seem to imply that the default_group is
;; automatically created if SC is started from its GUI, not
;; if it's started from cmd-line? Check that.
(define standard-group (make-unused-group-id))
(define send-effect-group (make-unused-group-id))
(define recording-group (make-unused-group-id))

;; Scratch audio bus that's overwritten by successive source and fx synths.
;; TODO: just the defaults! How do I find out if SC has been started with
;; a different bus? Don't want scratch to clash with anything.
(define num-audio-buses 1024)
(define scratch-audio-bus (dec num-audio-buses))

;;-----------------------------------------------------------------
;; Poke the server to check whether it's available.
;; If it is, reset it to a blank state: free all synths, add our main groups.
(let ([fail-msg "\nSuperCollider not found."])
  (sleep-secs 0.2)
  (sc/reset sc3) ;; Blank server with default group 1
  (sleep-secs 0.2)
  (with-exception-handler
   (lambda (x) (when (error? x) (error "init" fail-msg)))
   (lambda () (sc/server-sample-rate-nominal sc3)))
  (send-synth sc3 voice-group-out-synth
    (letc ([:in 0 ir])
      (mrg2 (out 0 (stereo-in :in))
            (replace-out :in (silence 2)))))
  (create-voice-group standard-group)
  (create-group send-effect-group sc/add-to-tail default-group)
  (create-group recording-group sc/add-to-tail default-group)
  (sleep-secs 0.125))

;;-----------------------------------------------------------------
;; Send a timestamped OSC bundle to launch a new synth with some arguments.
;; Usually there's no need to specify a synth-id unless you know what you're doing.
(define* (play-when name t group arg-pairs 
                    [/opt (synth-id -1) (target sc/add-to-head)])
  (send-bundle t (list (sc/s-new0 name synth-id target group)
                       (sc/n-set synth-id arg-pairs))))

;; Sends a control change to all the voices in group.
(define (control-when t group arg-pairs)
  (send-bundle t (list (sc/n-set group arg-pairs))))

;; Add an effect to all the voices in group.
(define (fx-when name t group arg-pairs)
  (let ([out-synth-id (voice-group-out-id group)])
    (play-when name t out-synth-id arg-pairs -1 sc/add-before)))

;; Useful for quick testing of synthdefs
(define (play-now name arg-pairs)
  (play-when name (sc/utc) standard-group arg-pairs))

;; Much like play-when, but adds to tail
(define (start-send-effect name . arg-pairs)
  (send-bundle (+ (sc/utc) 0.25) ;; Avoid confusing 'late' messages
               (list (sc/s-new0 name -1 sc/add-to-tail send-effect-group)
                     (sc/n-set -1 arg-pairs))))

;; Kills a running synth immediately (may cause clicks)
(define* (stop-synth synth-id [/opt (t (sc/utc))])
  (send-bundle t (list (sc/n-free1 synth-id))))
