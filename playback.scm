;; -*- geiser-scheme-implementation: chez-*-
;; This is the place where patterns are registered with a playback
;; thread. The thread periodically pulls events from the patterns and
;; pushes them on to our sound engine back-ends (e.g. OSC , MIDI).
(library (playback)
  (export
    bpm->mps bpm->spm
    measures->secs secs->measures
    secs-until sleep-secs
    semaphore semaphore? make-semaphore
    semaphore-val semaphore-mutex semaphore-cond
    start-waiting stop-waiting
    start-suspendable-thread)

  (import (context) (rsc3) (sosc)
          (except (scheme) reset random)
          (only (srfi s1 lists) alist-cons alist-delete))

  ;;------------------------------------------------
  ;; Some useful functions for dealing with time.

  ;; Convert bpm to measures per second (mps)
  (define (bpm->mps bpm) (/ bpm 60 4))
  ;; Convert bpm to seconds per measure
  (define (bpm->spm bpm) (/ 1 (bpm->mps bpm)))
  ;; Convert a value in measures to seconds
  (define (measures->secs m bpm) (* m (bpm->spm bpm)))
  ;; Convert a value in seconds to measures
  (define (secs->measures s bpm) (* s (bpm->mps bpm)))

  (define (secs-until beat current-beat bpm)
    (/ (- beat current-beat) (bpm->mps bpm)))

  (define (sleep-secs secs)
    (let* ([secs-whole (exact (truncate secs))]
           [ns (exact (truncate (* (- secs secs-whole) 10e8)))])
      (sleep (make-time 'time-duration ns secs-whole))))

  ;;------------------------------------------------
  ;; Infrastructure for a special playback thread. This will wake up
  ;; periodically (according to bpm and chunk-size) and send the next
  ;; chunk of event information to SuperCollider.
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
            (sleep-secs chunk-secs)
            (loop)))))))
  )
