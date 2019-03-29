;; -*- geiser-scheme-implementation: chez-*-
(load "libs.scm")

;;-----------------------------------------------------------------
;; Some SuperCollider setup: a default server, synthdef & event sender.
;; TODO: blocks if SuperCollider isn't running!! 
(define sc3 (udp:open "127.0.0.1" 57110))
(send sc3 (g-new1 1 add-to-tail 0))

(define (play-when name t . arg-pairs)
  (let ([args (list (s-new0 name -1 add-to-tail 1)
		    (n-set -1 arg-pairs))])
    (send sc3 (bundle t args))))

(define (play-now name . arg-pairs)
  (apply play-when name (utc) arg-pairs))

(load "synthdefs.scm")

;;-----------------------------------------------------------------
;; Plays a event at the right time in the future.
;; TODO: adjust latency based on frame jitter
(define (play-event event current-beat)
  (define (entry-convert pair)
    (cons (symbol->string (car pair)) (cdr pair)))
  (let* ([event (preprocess-event event)]
	 [inst (event-get event :inst "sine-grain")]
	 [beat (event-beat event)]
	 [until (secs-until beat current-beat bpm)]
	 [t (+ (utc) until playback-latency)])
    (apply play-when inst t (map entry-convert (event-clean event)))))

;; Preprocess an event. Computes frequency for events using the harmony
;; system. Gives sampler instrument to events with samples. 
(define (preprocess-event event)
  (cond
   ((event-get event :sample #f)
    (event-set event :inst (event-get event :inst "sampler")))
   (else
    (event-with-freq event))))

;; Our test pattern for the moment. Redefine for fun and profit!
(define p1 (in* 1))
(define (reset-p1) (define-top-level-value 'p1 (in* 1)))

;; Adds custom priting of contexts.
(record-writer (type-descriptor context) context-print) 

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
