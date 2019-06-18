(define pattern-dict (make-pattern-dict))

(define-syntax pattern
  (syntax-rules ()

    ((_ name play-now? p)
     (begin (define name p)
            (when play-now?
              (add-pattern pattern-dict 'name p))))

    ((_ name p)
     (pattern name #t p))))

(define (pattern-form? datum)
  (and (unsafe-list? datum) (eq? 'pattern (car datum))))

(define-syntax start
  (syntax-rules ()

    ((_) (start-playhead))

    ((_ name) (add-pattern pattern-dict 'name name))))

(define-syntax stop
  (syntax-rules ()

    ((_) (stop-playhead))

    ((_ name) (remove-pattern pattern-dict 'name))))

(define (pause) (pause-playhead)) ;; for symmetry
(define (clear-all) (clear-patterns pattern-dict))

(define (print-patterns start end)
  (define ctxt
    (fold-left (lambda (c p) (contexts-merge c (render p start end)))
               (make-empty-context start end)
               (list-patterns pattern-dict)))
  (define (process c)
    (event-clean (process-inst (context-event c))))
  (context-map process ctxt))

;;------------------------------------------------------------------
(define (stop-patterns-in-file file-path)
  (for-each (lambda (p) (remove-pattern pattern-dict p))
            (list-patterns-in-file file-path pattern-form?)))

(define (files-with-playing-patterns root-path)
  (list-files-with-playing-patterns root-path pattern-dict pattern-form?))