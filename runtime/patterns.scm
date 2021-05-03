(define pattern-dict (make-pattern-dict))

;; Starts/updates a pattern
(define-syntax pattern
  (syntax-rules ()
    ((_ (name q ...) p ps ...)
     (begin
       (println "Quantised change not implemented yet...") ;; TODO
       (pattern name p ps ...)))

    ((_ name p)
     (begin
       (define name p)
       (add-pattern pattern-dict 'name p)))

    ((_ name p ps ...)
     (pattern name (part p ps ...)))))

;; Another, more explicit way to stop a pattern:
(define-syntax stop
  (syntax-rules (pattern)
    ((_)
     (clear-patterns pattern-dict))

    ((_ pattern (name q ...) ys ...)
     (begin
       (println "Quantised change not implemented yet") ;; TODO
       (stop pattern name)))

    ((_ pattern name xs ...)
     (remove-pattern pattern-dict 'name))

    ((_ name ...)
     (begin (stop pattern name) ...))))

;;------------------------------------------------------------------
(define (pattern-form? datum)
  (and (unsafe-list? datum) (eq? 'pattern (car datum))))

(define (stop-patterns-in-file file-path)
  (for-each (lambda (p) (remove-pattern pattern-dict p))
            (list-patterns-in-file file-path pattern-form?)))

(define (files-with-playing-patterns root-path)
  (list-files-with-playing-patterns root-path pattern-dict pattern-form?))

(define (pattern-names)
  (list-pattern-names pattern-dict))

;;------------------------------------------------------------------
;; Tag these at the top level to disambiguate pattern syntax.
(tag-sdef-callable quote)
(tag-sdef-callable lambda)
