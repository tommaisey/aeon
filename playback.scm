;; -*- geiser-scheme-implementation: chez-*-
;; This is the place where patterns are registered with a playback
;; thread. The thread periodically pulls notes from the patterns and
;; pushes them on to our sound engine back-ends (e.g. OSC , MIDI).
(library (playback)
  (export
   add-pattern
   remove-pattern
   semaphore semaphore? make-semaphore
   semaphore-val semaphore-mutex semaphore-cond
   start-waiting stop-waiting
   start-suspendable-thread)
  
  (import (note-dsl) (context) (rsc3) (sosc)
	  (except (scheme) reset random)
	  (only (srfi s1 lists) alist-cons alist-delete))

  (define running-patterns '()) ; alist

  ;; A pattern being a function that takes and returns a context.
  ;; This will only play if `start-playback` has been called.
  (define (add-pattern pattern uid)
    (set! running-patterns
      (alist-cons uid pattern (alist-delete uid running-patterns))))

  (define (remove-pattern uid)
    (set! running-patterns (alist-delete uid running-patterns)))

  ;;------------------------------------------------
  ;; Infrastructure for a special playback thread. This will wake up
  ;; periodically (according to bpm and chunk-size) and send the next
  ;; chunk of note information to SuperCollider.
  (define-record-type semaphore
    (fields (mutable val semaphore-val set-semaphore-val!)
	    (immutable mutex)
	    (immutable cond))
    (protocol
     (lambda (new)
       (lambda () (new #t (make-mutex) (make-condition))))))

  (define (set-semaphore sem val)
    (with-mutex (semaphore-mutex sem)
      (set-semaphore-val! sem val)))
  
  (define (set-and-signal-semaphore sem val)
    (with-mutex (semaphore-mutex sem)
      (set-semaphore-val! sem val)
      (condition-signal (semaphore-cond sem))))

  (define (start-waiting sem) (set-semaphore sem #t))
  (define (stop-waiting sem) (set-and-signal-semaphore sem #f))

  ;; Allows introduction of rebindable values for process-fn and
  ;; chunk-secs, so you can use global values and change the
  ;; loop's callback interactively.
  (define-syntax start-suspendable-thread
    (syntax-rules ()
      ((_ process-fn chunk-secs sem)
       (fork-thread
       (lambda ()
	 (let loop ()
	   (with-mutex (semaphore-mutex sem)
	     (when (semaphore-val sem)
	       (condition-wait
		(semaphore-cond sem)
		(semaphore-mutex sem))))
	   (process-fn)
	   (let* ([secs chunk-secs]
		  [secs-whole (exact (truncate secs))]
		  [ns (exact (truncate (* (- secs secs-whole) 10e8)))])
	     (sleep (make-time 'time-duration ns secs-whole)))
	   (loop)))))))
  )
