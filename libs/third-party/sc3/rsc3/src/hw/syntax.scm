;; int -> maybe (float -> float) -> (node -> node)
(define mk-unary-operator
  (lambda (s f)
    (lambda (a)
      (if (and (number? a)
	       f)
	  (f a)
	  (construct-ugen "UnaryOpUGen" '() (list1 a) '() 1 s (make-uid 0))))))

;; int -> maybe (float -> float -> float) -> (node -> node -> node)
(define mk-binary-operator
  (lambda (s f)
    (lambda (a b)
      (if (and (number? a)
	       (number? b)
	       f)
	  (f a b)
	  (construct-ugen "BinaryOpUGen" '() (list2 a b) '() 1 s (make-uid 0))))))

;; string -> [symbol] -> int ~> (ugen ... -> ugen)
(define-syntax mk-filter
  (syntax-rules ()
    ((_ m (i ...) o)
     (lambda (i ...)
       (construct-ugen m '() (list i ...) '() o 0 (make-uid 0))))))

;; string -> [symbol] ~> (int -> ugen ... -> ugen)
(define-syntax mk-filter-n
  (syntax-rules ()
    ((_ m (i ...))
     (lambda (nc i ...)
       (if (not (integer? nc))
	   (error "mk-filter-n" "illegal channel count" 'n nc)
	   '())
       (let ((l (list i ...)))
	 (construct-ugen m '() l '() nc 0 (make-uid 0)))))))

;; string -> [symbol] -> int ~> (ugen ... -> ugen)
(define-syntax mk-filter-mce
  (syntax-rules ()
    ((_ m (i ... v) o)
     (lambda (i ... v)
       (construct-ugen m '() (list i ...) v o 0 (make-uid 0))))))

;; string -> [symbol] -> int ~> (ugen ... -> ugen)
(define-syntax mk-filter-id
  (syntax-rules ()
    ((_ m (i ...) o)
     (lambda (i ...)
       (construct-ugen m '() (list i ...) '() o 0 (unique-uid))))))

;; string -> [symbol] -> int -> int ~> (ugen ... -> ugen)
(define-syntax mk-filter-k
  (syntax-rules ()
    ;; k = keyed input
    ((_ m (i ...) o k)
     (lambda (i ...)
       (let ((l (list i ...)))
	 (construct-ugen m (rate-of (list-ref l k)) l '() o 0 (make-uid 0)))))))

;; string -> [symbol] -> int ~> (rate -> ugen ... -> ugen)
(define-syntax mk-oscillator
  (syntax-rules ()
    ((_ m (i ...) o)
     (lambda (r i ...)
       (construct-ugen m r (list i ...) '() o 0 (make-uid 0))))))

;; string -> [symbol] ~> (int -> rate -> ugen ... -> ugen)
(define-syntax mk-oscillator-n
  (syntax-rules ()
    ((_ m (i ...))
     (lambda (nc r i ...)
       (if (not (integer? nc))
	   (error "mk-oscillator-n" "illegal channel count:" 'n nc)
	   '())
       (let ((l (list i ...)))
	 (construct-ugen m r l '() nc 0 (make-uid 0)))))))

;; string -> [symbol] -> int ~> (rate -> ugen ... -> ugen)
(define-syntax mk-oscillator-mce
  (syntax-rules ()
    ((_ m (i ... v) o)
     (lambda (r i ... v)
       (construct-ugen m r (list i ...) v o 0 (make-uid 0))))))

;; string -> [symbol] -> int ~> (rate -> ugen ... -> ugen)
(define-syntax mk-oscillator-id
  (syntax-rules ()
    ((_ m (i ...) o)
     (lambda (r i ...)
       (construct-ugen m r (list i ...) '() o 0 (unique-uid))))))

;; string -> [symbol] -> int -> rate ~> (ugen ... -> ugen)
(define-syntax mk-specialized
  (syntax-rules ()
    ((_ m (i ...) o r)
     (lambda (i ...)
       (construct-ugen m r (list i ...) '() o 0 (make-uid 0))))))

;; string -> int -> rate ~> ugen
(define-syntax mk-specialized-c
  (syntax-rules ()
    ((_ m o r)
     (construct-ugen m r nil '() o 0 (make-uid 0)))))

;; string -> [symbol] -> int -> rate ~> (ugen ... -> ugen)
(define-syntax mk-specialized-mce
  (syntax-rules ()
    ((_ m (i ... v) o r)
     (lambda (i ... v)
       (construct-ugen m r (list i ...) v o 0 (make-uid 0))))))

;; string -> [symbol] -> rate ~> (int -> ugen ... -> ugen)
(define-syntax mk-specialized-n
  (syntax-rules ()
    ((_ m (i ...) r)
     (lambda (nc i ...)
       (if (not (integer? nc))
	   (error "mk-specialized-n" "illegal channel count:" 'n nc)
	   '())
       (let ((l (list i ...)))
	 (construct-ugen m r l '() nc 0 (make-uid 0)))))))

;; string -> [symbol] -> int -> rate ~> (ugen ... -> ugen)
(define-syntax mk-specialized-id
  (syntax-rules ()
    ((_ m (i ...) o r)
     (lambda (i ...)
       (construct-ugen m r (list i ...) '() o 0 (unique-uid))))))

(define-syntax mk-specialized-n-id
  (syntax-rules ()
    ((_ m (i ...) r)
     (lambda (nc i ...)
       (if (not (integer? nc))
	   (error "mk-specialized-n" "illegal channel count:" 'n nc)
	   '())
       (let ((l (list i ...)))
	 (construct-ugen m r l '() nc 0 (unique-uid)))))))

;; string -> [symbol] -> int -> rate ~> (ugen ... -> ugen)
(define-syntax mk-specialized-mce-id
  (syntax-rules ()
    ((_ m (i ... v) o r)
     (lambda (i ... v)
       (construct-ugen m r (list i ...) v o 0 (unique-uid))))))

