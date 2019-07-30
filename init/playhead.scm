;;-----------------------------------------------------------------
(define bpm 100)
(define playback-thread #f)
(define playback-chunk 1/8) ; 1/8th beat for now
(define playback-thread-semaphore (make-semaphore))
(define playback-latency 0.2)

;; State used to mitigate timing jitter in callbacks:
(define last-process-time #f) ;; time of last callback, utc
(define last-process-beat 0)  ;; time of last callback, beats
(define jitter-overlap 1/32)  ;; extra time to render each block
(define rendered-point #f)    ;; musical time that has been sent to SC

;; Called regularly by the playback thread. It renders events in
;; chunks whose length are determined by playback-chunk, plus a
;; little extra ('jitter-overlap') to allow for the callback to
;; happen late.
(define (process-chunk)
  (let ([t (utc)])

    ;; Dispatches all the events that were rendered.
    (define (pattern-player now-beat start end)
      (lambda (p)
        (for-each (lambda (e) (play-event e now-beat t))
                  (context-events-next (render p start end)))))

    (guard (x [else (handle-error x)])
      (let* ([now (+ last-process-beat (beats-since-last-process t))]
             [start (or rendered-point now)]
             [end (+ now playback-chunk jitter-overlap)]
             [player (pattern-player now start end)])
        (iterate-patterns pattern-dict player)
        (set! last-process-time t)
        (set! last-process-beat now)
        (set! rendered-point end)))))

;; Only creates new thread if one isn't already in playback-thread.
(define (start-thread semaphore)
  (when (not playback-thread)
    (set! playback-thread
          (start-suspendable-thread
           process-chunk (* playback-chunk (bpm->spm bpm)) semaphore))))

(define (start-playhead)
  (start-thread playback-thread-semaphore)
  (stop-waiting playback-thread-semaphore)
  (playhead-sync-info))

(define (pause-playhead)
  (start-waiting playback-thread-semaphore)
  (send sc3 clear-sched)
  (set! rendered-point #f)
  (set! last-process-time #f)
  (playhead-sync-info))

(define (stop-playhead)
  (pause-playhead)
  (set! last-process-beat 0)
  (playhead-sync-info))

(define (playing?)
  (not (waiting? playback-thread-semaphore)))

(define (beats-since-last-process utc-time)
  (if (not last-process-time) 0
      (secs->measures (- utc-time last-process-time) bpm)))

(define (playhead-sync-info)
  (let ([now (+ last-process-beat (beats-since-last-process (utc)))])
    (println
     (format "(playhead-sync ~A (position ~A) (mps ~A))"
             (if (playing?) 'playing 'stopped) now (bpm->mps bpm)))))

(define (set-bpm! n)
  (set! bpm n)
  (let ([e (make-event 0 
                       (:tempo (bpm->mps n))
                       (:control "tempo")
                       (:group bus-effect-group))])
    (playhead-sync-info)
    (play-event e 0)))

(define (handle-error condition)
  (let ([p (console-output-port)])
    (display-condition condition p)
    (newline p)
    (flush-output-port p)
    (clear-patterns pattern-dict)))