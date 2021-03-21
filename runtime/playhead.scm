;;-----------------------------------------------------------------
(define bpm 115)
(define playback-thread #f)
(define playback-chunk 1/8) ; 1/8th beat for now
(define playback-thread-semaphore (make-semaphore))
(define playback-latency 0.2)

;; State used to mitigate timing jitter in callbacks:
(define last-process-time #f) ;; time of last callback, utc
(define last-process-beat 0)  ;; time of last callback, beats
(define jitter-overlap 1/32)  ;; extra time to render each block
(define rendered-point #f)    ;; musical time that has been sent to SC

;; Find out how many beats have elapsed since the last process-chunk.
(define (beats-since-last-process utc-time)
  (if (not last-process-time) 0
      (secs->measures (- utc-time last-process-time) bpm)))

;; Called regularly by the playback thread. It renders events in
;; chunks whose length are determined by playback-chunk, plus a
;; little extra ('jitter-overlap') to allow for the callback to
;; happen late.
(define (process-chunk)
  (let ([t (sc/utc)])

    ;; Make a dispatcher for all the events rendered from a pattern.
    (define (pattern-player beat-now start end)
      (lambda (p)
        (for-each (lambda (e) (play-event e beat-now t))
                  (context-events-next (render-arc p start end)))))

    (guard (x [else (handle-error x)])
      (let* ([now (+ last-process-beat (beats-since-last-process t))]
             [start (or rendered-point now)]
             [end (+ now playback-chunk jitter-overlap)]
             [player (pattern-player now start end)])
        (iterate-patterns pattern-dict player)
        (update-recording now (make-arc start end) t)
        (set! last-process-time t)
        (set! last-process-beat now)
        (set! rendered-point end)))))

;; TODO: clear only the offending pattern on error
(define (handle-error condition)
  (let ([p (console-output-port)])
    (display-condition condition p)
    (newline p)
    (flush-output-port p)
    (clear-patterns pattern-dict)))

;; Only creates new thread if one isn't already in playback-thread.
(define (start-thread semaphore)
  (when (not playback-thread)
    (set! playback-thread
          (start-suspendable-thread
           process-chunk (* playback-chunk (bpm->spm bpm)) semaphore))))

(define (play)
  (start-thread playback-thread-semaphore)
  (stop-waiting playback-thread-semaphore)
  (pipe-out (playhead-sync-info)))

(define (pause)
  (start-waiting playback-thread-semaphore)
  (so/send sc3 sc/clear-sched)
  (cancel-recording)
  (set! rendered-point #f)
  (set! last-process-time #f)
  (pipe-out (playhead-sync-info)))

(define* (rewind [/opt (keep-playing #t)])
  (pause)
  (set! last-process-beat 0)
  (when keep-playing (play)))

(define (playing?)
  (not (waiting? playback-thread-semaphore)))

(define (set-bpm! n)
  (set! bpm n)
  (pipe-out (playhead-sync-info))
  (play-event (make-event 0 
                          :tempo (bpm->mps n)
                          :control "tempo"
                          :group send-effect-group) 0))
