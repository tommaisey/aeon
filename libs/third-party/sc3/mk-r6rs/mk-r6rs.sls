(library (mk-r6rs)
  (export mk-r5rs mk-r6rs mk-r7rs)
  (import (chezscheme))
  ;; Change to 'write' for (rnrs) compatiblilty
  (define write-form pretty-print)

  (define concat
    (lambda (l)
      (fold-right append (list) l)))

  (define id
    (lambda (x)
      x))

  (define parse-field
    (lambda (f)
      (if (symbol? f)
          (list 'immutable f)
          (list (car f) (cadr f)))))

  ;; (symbol-append 'join '- 'me)
  (define symbol-append
    (lambda l
      (string->symbol (apply string-append (map symbol->string l)))))

  ;; (gen-r6rs-record-type '(define-record-type rec (fields f g)))
  ;; (gen-r6rs-record-type '(define-record-type rec (fields (mutable f) g)))
  (define gen-r6rs-record-type
    (lambda (x)
      (let* ((r (list-ref x 1))
             (fs (cdr (list-ref x 2)))
             (fss (map parse-field fs)))
	(append
	 (list r (symbol-append 'make- r) (symbol-append r '?))
	 (concat (map (lambda (s)
			(let* ((st (car s))
                               (nm (cadr s))
                               (ac (symbol-append r '- nm)))
                          (if (eq? st 'mutable)
                              (list ac (symbol-append ac '- 'set!))
                              (list ac))))
                      fss))))))

  ;; (gen-srfi-record-type '(srfi:define-record-type rec (make-rec f g) rec? (f rec-f) (g rec-g)))
  (define gen-srfi-record-type
    (lambda (x)
      (let* ((r (list-ref x 1))
             (mk (car (list-ref x 2)))
             (pr (list-ref x 3))
             (fs (map cadr (cddddr x))))
	(append (list r mk pr) fs))))

  (define identifiers
    (lambda (l)
      (let ((f (lambda (x)
		 (if (list? x)
		     (let ((d (list-ref x 0)))
		       (cond ((member d (list 'define 'define-syntax))
			      (list (list-ref x 1)))
			     ((equal? d 'define-record-type)
			      (gen-r6rs-record-type x))
			     ((equal? d 'srfi:define-record-type)
			      (gen-srfi-record-type x))
			     (else #f)))
		     #f))))
	(concat (filter id (map f l))))))

  (define all-values
    (letrec ((f (lambda (p)
		  (let ((x (read p)))
		    (if (eof-object? x)
			(list)
			(cons x (f p)))))))
      (lambda (fn)
	(call-with-input-file fn
	  (lambda (p)
	    (f p))))))

  (define export-list
    (lambda (s l)
      (cons s (identifiers l))))

  (define excluding
    (lambda (xs s)
      (filter
	(lambda (x)
	  (not (member (car (cdr x)) s)))
	xs)))

  (define mk-r5rs
    (lambda (srcs dst)
      (call-with-output-file dst
	(lambda (p)
	  (let* ((xs (concat (map all-values srcs))))
	    (map (lambda (x)
                   (write-form x p)
                   (newline p))
		 xs))))))

  (define mk-r6rs
    (lambda (lib srcs dst imports are-private to-exclude)
      (call-with-output-file dst
	(lambda (p)
          (let* ((xs (concat (map all-values srcs)))
		 (xs-p (excluding xs (append are-private to-exclude))))
            (display "#!r6rs" p)
            (newline p)
            (write-form
             `(library
		  ,lib
                ,(export-list 'export xs-p)
                (import ,@imports)
                ,@xs)
             p))))))

  (define mk-r7rs
    (lambda (lib srcs dst imports are-private to-exclude)
      (call-with-output-file dst
	(lambda (p)
          (let* ((xs (concat (map all-values srcs)))
		 (xs-p (excluding xs (append are-private to-exclude))))
            (write-form
             `(define-library
		  ,lib
                ,(export-list 'export xs-p)
                (import ,@imports)
                (begin ,@xs))
             p))))))

)
