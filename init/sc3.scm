;;-----------------------------------------------------------------
;; Some SuperCollider setup: a default server, synthdef & event sender.
(println "Connecting to SuperCollider...")
(define sc3 (so/udp:open "127.0.0.1" 57110))

;; Send a timestamped OSC bundle. If you want to block
;; until the arrival of a response, supply return-msg-head (usually "/done")
(define (send-bundle t args)
  (so/send sc3 (so/bundle t args)))

;; Send a timestamped OSC bundle to launch a new synth with some arguments.
;; Using a group other than main-group allows control of synths while they run.
;; Usually there's no need to specify a synth-id unless you know what you're doing.
(define* (play-when name t group arg-pairs [/opt (synth-id -1)])
  (send-bundle t (list (sc/s-new0 name synth-id sc/add-to-head group)
                       (sc/n-set synth-id arg-pairs))))

;; Useful for quick testing of synthdefs
(define (play-now name . arg-pairs)
  (play-when name (sc/utc) standard-group arg-pairs))

;; Sends a control change to all the voices in group.
(define (control-when t group arg-pairs)
  (send-bundle t (list (sc/n-set group arg-pairs))))

;; Much like play-when, but adds to tail
(define (start-bus-effect name . arg-pairs)
  (send-bundle (+ (sc/utc) 0.25) ;; Avoid confusing 'late' messages
               (list (sc/s-new0 name -1 sc/add-to-tail bus-effect-group)
                     (sc/n-set -1 arg-pairs))))

;; Kills a running synth immediately (may cause clicks)
(define* (stop-synth synth-id [/opt (t (sc/utc))])
  (send-bundle t (list (sc/n-free1 synth-id))))

;;-----------------------------------------------------------------
;; We keep a list of all the groups that have been created so
;; we don't spam new group commands to SC.
(define known-groups (list))
(define (known-group? id) (member id known-groups))

(define default-group 1) ;; See SC docs on default_group
(define group-id-counter default-group)

(define (make-unused-group-id)
  (set! group-id-counter (+ group-id-counter 1))
  group-id-counter)

(define (create-group id order target)
  (when (and (number? id)
             (> id 1)
             (not (known-group? id)))
    (set! known-groups (cons id known-groups))
    (so/send sc3 (sc/g-new1 id order target))))

;; n.b. SC docs seem to imply that the default_group is
;; automatically created if SC is started from its GUI, not
;; if it's started from cmd-line? Check that.
(define standard-group (make-unused-group-id))
(define bus-effect-group (make-unused-group-id))
(define recording-group (make-unused-group-id))

;;-----------------------------------------------------------------
;; Poke the server to check whether it's available.
;; If it is, reset it to a blank state: free all synths, add our main groups.
(let ([fail-msg "\nSuperCollider unavailable. Is it running?"])
  (sleep-secs 0.25)
  (sc/reset sc3) ;; Blank server with default group 1
  (sleep-secs 0.25)
  (with-exception-handler
   (lambda (x) (when (error? x) (error "initialisation" fail-msg)))
   (lambda () (sc/server-sample-rate-nominal sc3)))
  (create-group standard-group sc/add-to-head default-group)
  (create-group bus-effect-group sc/add-to-tail default-group)
  (create-group recording-group sc/add-to-tail default-group))
