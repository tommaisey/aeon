;; -*- geiser-scheme-implementation: chez-*-
(load "libs.scm")

;; A default test pattern. Redefine for fun and profit!
(define p1 (in* 1))
(define (reset-p1) (set! p1 (in* 1)))

;; Adds custom priting of contexts.
(record-writer (type-descriptor context) context-print)

;;-----------------------------------------------------------------
;; Some SuperCollider setup: a default server, synthdef & event sender.
;; TODO: this blocks if SuperCollider isn't running!! 
(define sc3 (udp:open "127.0.0.1" 57110))

;; Helper for sending timestamped OSC bundles
(define (send-bundle t args)
  (send sc3 (bundle t args)))

;; Groups other than main-group can be used to control
;; groups of voices after they have started.
(define (play-when name t group arg-pairs)
  (send-bundle t (list (s-new0 name -1 add-to-head group)
		       (n-set -1 arg-pairs))))

;; Useful for quick testing of synthdefs
(define (play-now name . arg-pairs)
  (play-when name (utc) main-group arg-pairs))

;; Sends a control change to all the voices in group.
(define (control-when t group arg-pairs)
  (send-bundle t (list (n-set group arg-pairs))))

;; Much like play-when, but adds to tail
(define (start-bus-effect name . arg-pairs)
  (send-bundle (+ (utc) 0.25) ;; Avoid confusing 'late' messages
	       (list (s-new0 name -1 add-to-tail bus-effect-group)
		     (n-set -1 arg-pairs))))

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
    (send sc3 (g-new1 id order default-group))))

;; n.b. SC docs seem to imply that the default_group is
;; automatically created if SC is started from its GUI, not
;; if it's started from cmd-line? Check that.
(define standard-group (make-unused-group-id))
(define bus-effect-group (make-unused-group-id))
;; (create-group default-group add-after 0)
(create-group standard-group add-to-head default-group)
(create-group bus-effect-group add-to-tail default-group)

(load "synthdefs.scm")

;;-----------------------------------------------------------------
;; Plays a event at the right time in the future.
;; TODO: adjust latency based on frame jitter
(define (play-event event current-beat)
  (define (entry-convert pair)
    (cons (symbol->string (car pair)) (cdr pair)))
  
  (let ([event (preprocess-event event)])
    (alist-let event ([beat    :beat 0]
		      [inst    :inst "sine-grain"]
		      [group   :group default-group]
		      [control :control #f]
		      [sustain :sustain #f])
      (let* ([until (secs-until beat current-beat bpm)]
	     [time (+ (utc) until playback-latency)]
	     [args (map entry-convert (event-clean event))])
	(create-group group add-before default-group)
	(if control
	    (if (or (not (number? group)) (<= group 1))
		(println "Error: control event didn't specify a valid group.")
		(control-when time group args))
	    (play-when inst time group args))))))

;; Preprocess an event. Computes frequency for events using the harmony
;; system. Gives sampler instrument to events with samples. 
(define (preprocess-event event)
  (process-times (process-inst event)))

;; If it looks like a sampler event, sets the right inst if it's
;; missing. If it looks like a freq event, computes freq.
(define (process-inst event)
    (if (event-get event :sample #f)
	(event-set event :inst (event-get event :inst "sampler"))
	(event-with-freq event)))

;; Converts any key found in the 'tempo-dependent-keys' alist
;; from measures to seconds, for prior to sending to SC.
(define (process-times event)
  (define (convert event entry)
    (if (cdr entry)
	(event-set event (car entry) (measures->secs (cdr entry) bpm))
	event))
  (let ([vals (event-get-multi event tempo-dependent-keys)])
    (fold-left convert event vals)))

;;-----------------------------------------------------------------
;; A thread that wakes up every playback-chunk beats to call (process-chunk)
;; This essentially controls playback for now.
(define bpm 100)
(define playback-thread #f)
(define playback-chunk 1/4) ; 1 quarter measure for now
(define playback-thread-semaphore (make-semaphore))
(define playback-time 0)
(define playback-latency 0.2)

;; Called each chunk of time by the playback thread.  
(define (process-chunk)
  (guard (x [else (handle-error x)])
    (let* ([t1 playback-time]
	   [t2 (+ t1 playback-chunk)]
	   [c (render-arc p1 t1 t2)]
	   [events (context-events-next c)])
      (for-each (lambda (n) (play-event n t1)) events)
      (set! playback-time t2))))

;; Only creates new thread if one isn't already in playback-thread.
(define (start-thread sem)
  (when (not playback-thread)
    (set! playback-thread
      (start-suspendable-thread
       process-chunk (* playback-chunk (bpm->spm bpm)) sem))))

(define (start)
  (start-thread playback-thread-semaphore)
  (stop-waiting playback-thread-semaphore))

(define (pause)
  (start-waiting playback-thread-semaphore))

(define (stop)
  (pause)
  (set! playback-time 0))

(define (set-bpm! n)
  (set! bpm n)
  (let ([e (make-event 0 (:tempo (bpm->mps n))
		         (:control "tempo")
		         (:group bus-effect-group))])
    (play-event e 0)))

(set-bpm! bpm) ;; Ensure fx synths have tempo

(define (handle-error condition)
  (let ([p (console-output-port)])
    (display-condition condition p)
    (newline p)
    (flush-output-port p)
    (reset-p1)))
