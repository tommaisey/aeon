;; -*- geiser-scheme-implementation: chez-*-
(load "libs.scm")

;;-----------------------------------------------------------------
;; Some SuperCollider setup: a default server, synthdef & event sender.
;; TODO: blocks if SuperCollider isn't running!! 
(define s (udp:open "127.0.0.1" 57110))
(send s (g-new1 1 add-to-tail 0))

(send-synth
 s "sine-grain"
 (letc ([freq 440] [attack 0.01] [length 1])
   (let* ([d (env-perc attack length 1 (list -4 -4))]
	  [e (env-gen kr 1 0.2 0 1 remove-synth d)])
     (out 0 (mul e (sin-osc ar freq 0))))))

(define (play-at name t . arg-pairs)
  (let ([args (list (s-new0 name -1 add-to-tail 1)
		    (n-set -1 arg-pairs))])
    (send s (bundle t args))))

(define (play-now name . arg-pairs)
  (play-at name (utc) arg-pairs))

(define (sine-perc freq length)
  (play-now "sine-grain"
	    (cons "freq" freq)
	    (cons "length" length)))

;;-----------------------------------------------------------------
;; Plays a note at the right time in the future.
(define (play-note note current-beat)
  (define (entry-convert pair)
    (cons (symbol->string (car pair)) (cdr pair)))
  (let* ([inst (note-get note 'inst "sine-grain")]
	 [beat (note-beat note)]
	 [until (secs-until beat current-beat bpm)]
	 [t (+ (utc) until playback-latency)]) ;; TODO: adjust latency based on frame jitter
    (apply play-at inst t (map entry-convert (note-clean note)))))

;; Our test pattern for the moment. Redefine for fun and profit!
(define (*1 context) context)

;; Called each chunk of time by the playback thread.  
(define (process-chunk)
  (let* ([t playback-time]
	 [nxt-t (+ t playback-chunk)]
	 [c (make-empty-context t nxt-t)])
    (for-each (lambda (n) (play-note n t)) (context-notes-next (*1 c)))
    (set! playback-time nxt-t)))

;;-----------------------------------------------------------------
;; A thread that wakes up every playback-chunk beats to call (process-chunk)
;; This essentially controls playback for now.
(define bpm 120)
(define playback-thread #f)
(define playback-chunk 1/4) ; 1 quarter note for now
(define playback-thread-semaphore (make-semaphore))
(define playback-time 0)
(define playback-latency 0.2)

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
