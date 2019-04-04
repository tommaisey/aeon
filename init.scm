;; -*- geiser-scheme-implementation: chez-*-
(load "libs.scm")

;; A default test pattern. Redefine for fun and profit!
(define p1 (in* 1))
(define (reset-p1) (set! p1 (in* 1)))

;; Adds custom priting of contexts.
(record-writer (type-descriptor context) context-print)

;;-----------------------------------------------------------------
;; Some SuperCollider setup: a default server, synthdef & event sender.
;; TODO: blocks if SuperCollider isn't running!! 
(define sc3 (udp:open "127.0.0.1" 57110))
(define main-group 1)
(send sc3 (g-new1 main-group add-to-tail 0))

;; Groups other than main-group can be used to control
;; groups of voices after they have started.
(define (play-when name t group arg-pairs)
  (let ([args (list (s-new0 name -1 add-to-tail group)
		    (n-set -1 arg-pairs))])
    (send sc3 (bundle t args))))

(define (control-when t group arg-pairs)
  (let ([args (list (n-set group arg-pairs))])
    (send sc3 (bundle t args))))

(define (play-now name . arg-pairs)
  (play-when name (utc) main-group arg-pairs))

;; We keep a list of all the groups that have been created so we don't
;; spam new group commands to SC.
(define known-groups (list))
(define (known-group? id) (member id known-groups))

(define (create-group id)
  (when (and (number? id)
	     (not (<= id 1))
	     (not (known-group? id)))
    (set! known-group (cons id known-groups))
    (send sc3 (g-new1 id add-to-tail 0))))

(define group-id-counter 1)

(define (make-unused-group-id)
  (set! group-id-counter (+ group-id-counter 1))
  group-id-counter)

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
		      [group   :group 1]
		      [control :control #f]
		      [sustain :sustain #f])
      (let* ([until (secs-until beat current-beat bpm)]
	     [time (+ (utc) until playback-latency)]
	     [args (map entry-convert (event-clean event))])
	(create-group group)
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

(define (handle-error condition)
  (let ([p (console-output-port)])
    (display-condition condition p)
    (newline p)
    (flush-output-port p)
    (reset-p1)))
