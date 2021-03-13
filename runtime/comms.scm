(define comms-pipe (make-parameter #f))

(define (open-comms-pipe path)
  (comms-pipe (open-file-output-port
               path
               (file-options no-fail)
               (buffer-mode block)
               (make-transcoder (utf-8-codec)))))

(define (pipe-out datum)
  (lest [p (comms-pipe)]
        (begin
          (put-datum p datum)
          (flush-output-port p))))

;;-------------------------------------------------------------
;; depends on playhead.scm
(define (playhead-sync-info)
  (let ([now (+ last-process-beat (beats-since-last-process (sc/utc)))])
    (list 'playhead-sync (if (playing?) 'playing 'stopped)
          (list 'position now)
          (list 'mps (bpm->mps bpm)))))

;;-------------------------------------------------------------
;; depends on playhead.scm
(define (context-serialised c)
  (list 'context
        (list 'arc (context-start c) (context-end c))
        (cons 'events (event-clean (context-events-next c)))))

;; depends on patterns.scm
(define (print-patterns start end)
  (define ctxt
    (fold-left (lambda (c p) (contexts-merge c (render-arc p start end)))
               (make-empty-context start end)
               (list-patterns pattern-dict)))
  (define (process c)
    (event-clean (process-inst (context-event c))))
  (context-serialised (context-map process ctxt)))

;; serialise a test render of a pattern
(define* (testp pattern-fn [/opt (start 0) (end 1)])
  (put-datum (current-output-port)
             (lif [c (render-arc pattern-fn start end)]
                  (context? c) (context-serialised c) c)))