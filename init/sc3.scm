;;-----------------------------------------------------------------
;; Some SuperCollider setup: a default server, synthdef & event sender.
(println "Connecting to SuperCollider...")
(define sc3 (so/udp:open "127.0.0.1" 57110))

;; Helper for sending timestamped OSC bundles
(define (send-bundle t args)
  (so/send sc3 (so/bundle t args)))

;; Groups other than main-group can be used to control
;; groups of voices after they have started.
(define (play-when name t group arg-pairs)
  (send-bundle t (list (sc/s-new0 name -1 sc/add-to-head group)
                       (sc/n-set -1 arg-pairs))))

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
             (not (< id 1))
             (not (known-group? id)))
    (set! known-groups (cons id known-groups))
    (so/send sc3 (sc/g-new1 id order target))))

;; n.b. SC docs seem to imply that the default_group is
;; automatically created if SC is started from its GUI, not
;; if it's started from cmd-line? Check that.
(define standard-group (make-unused-group-id))
(define bus-effect-group (make-unused-group-id))

(begin
  (sleep-secs 0.25)
  (sc/reset sc3) ;; Make sure we start with a blank server
  (sleep-secs 0.25)
  (with-exception-handler
   (lambda (x) (when (error? x) (error "initialisation" "\nSuperCollider unavailable. Is it running?")))
   (lambda () (sc/server-sample-rate-nominal sc3)))
  (create-group standard-group sc/add-to-head default-group)
  ;; Ensure fx synths have tempo
  (create-group bus-effect-group sc/add-to-tail default-group))