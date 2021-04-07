(define pattern-dict (make-pattern-dict))

(define-syntax pattern
  (syntax-rules ()
    ((_ name p)
     (begin
       (define name p)
       (add-pattern pattern-dict 'name p)))

    ((_ name p ps ...)
     (pattern name (o-> p ps ...)))))

(define-syntax start
  (syntax-rules ()
    ((_ name ...) 
     (begin (add-pattern pattern-dict 'name name) ...))))

(define-syntax stop
  (syntax-rules ()
    ((_)
     (clear-patterns pattern-dict))

    ((_ name ...)
     (begin (remove-pattern pattern-dict 'name) ...))))

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
