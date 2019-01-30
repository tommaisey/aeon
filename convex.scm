;; -*- geiser-scheme-implementation: chez-*-
(load "init.scm")

;; A SuperCollider test sound...
(send-synth
 s "sine-grain"
 (letc ([freq 440] [attack 0.01] [release 1])
   (let* ([d (env-perc attack release 1 (list -4 -4))]
	  [e (env-gen kr 1 0.1 0 1 remove-synth d)])
     (out 0 (mul e (sin-osc ar freq 0))))))

(define (play name . arg-pairs)
  (send s (bundle (utc)
		  (list (s-new0 name -1 add-to-tail 1)
			(n-set -1 arg-pairs)))))

;; Make a pair
(define-syntax npair
  (syntax-rules ()
    ((_ var) (cons (symbol->string 'var) var))))

(define (sine-perc freq release)
  (play "sine-grain"
	(npair freq)
	(npair release)))

(define (sine-smooth freq time)
  (play "sine-grain"
	(npair freq)
	(cons "attack" time)
	(cons "release" time)))

(define (process-chunk)
  (sine-perc 500 0.8))

;; A thread that wakes up every playback-chunk beats to call (process-chunk)
;; This essentially controls playback for now.
(define bpm 120)
(define playback-thread #f)
(define playback-chunk 1) ; 1 quarter note for now
(define playback-thread-semaphore (make-semaphore))

;; Only creates new thread if one isn't already in playback-thread.
(define (start-thread sem)
  (when (not playback-thread)
    (set! playback-thread
      (start-suspendable-thread
       process-chunk (* playback-chunk (/ 60 bpm)) sem))))

(define (start-playback)
  (start-thread playback-thread-semaphore)
  (stop-waiting playback-thread-semaphore))

(define (stop-playback)
  (start-waiting playback-thread-semaphore))
