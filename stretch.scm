
(library (stretch)
  (export stretch)
  (import (scheme) (utilities) (context))

  ;; Allows definition of nested lists without (quasi)quoting.
  ;; Lists that start with an identifier are evaluated.
  ;; Lists that don't start with an identifier are just a list.
  ;;
  ;; (auto-quasi (1 2 "hi" (+ 2 2) (5 (+ 5 5)))) => (1 2 "hi" 4 (5 10))
  (define-syntax auto-quasi
    (lambda (x)
      (syntax-case x ()
	((_ (v rest ...))
	 (identifier? (syntax v))
	 (syntax (v rest ...))) ; identifier first, call as lambda
	
	((_ (v ...))
	 (syntax (list (auto-quasi v) ...))) ;; make list
     
        ((_ v)
	 (syntax v))))) ;; normal value in a list

  ;; Any item in a pattern may be a raw value or a proc taking a context.
  (define (getv v context)
    (if (procedure? v) (v context) v))

  (define process-stretch-def
    (case-lambda
      ((t start end)
       (lambda (lst) (filter (lambda (x) (between-each x t start end))
			(map (lambda (y) (* t y)) lst))))

      ((t) (process-stretch-def t 0 t))
      (()  (process-stretch-def 1))))

  ;; Returns a list of x repeated n times.
  ;;
  ;; (subdiv 3 5) => (5 5 5)
  (define (subdiv n x)
    (let loop ([n n] [o '()])
      (if (= 0 n) o (loop (- n 1) (cons x o)))))
  
  ;; Turn a pattern-def into a list of time segments paired with the original
  ;; value of each item in the pattern. If do-subdiv is #t, numeric items
  ;; greater than 1 are considered instructions to subdivide the step by N.
  ;;
  ;; (psegment '[~ 3] 1 #f) => ((1/2 . ~) (1/2 . 3))
  ;; (psegment '[~ 3] 1 #t) => ((1/2 . ~) (1/6 . 3) (1/6 . 3) (1/6 . 3))
  (define (psegment p t do-subdiv)
    (let ([subseg (lambda (sub-p) (psegment sub-p (/ t (length p)) do-subdiv))])
      (cond
       ((pair? p)
	(merge-inner (map subseg p)))
       ((and do-subdiv (number? p) (> p 1))
	(subdiv p (cons (* t (/ 1 p)) p)))
       (else (cons t p)))))

  (define (pfill segs len start end map-seg flt-result)
    (define get-seg-t car)
    (define get-seg-v cdr)
    (if (and (> end start)
	     (pair? segs))
	(let loop ([s segs]
		   [t (exact (truncate (/ start len)))]
		   [o '()])
	  (cond
	   ((null? s) (loop segs t o))
	   ((>= t end) (reverse (filter flt-result o)))
	   (else
	    (loop (cdr s)
		  (+ t (get-seg-t (car s)))
		  (cons (map-seg t (get-seg-v (car s))) o)))))
	'()))
  
  (define (pfill-notes segs len start end)
    (define (make-note-or-rest t val)
      (if (eq? val '~) (list) (make-note t)))
    (define (ensure-inside note)
      (and (pair? note) (between (note-beat note) start end)))
    (pfill segs len start end make-note-or-rest ensure-inside))
  
  (define (build-event-pattern pattern-def stretch-def-args)
    (lambda (context)
      (let* ([times (range-start (context-range context))]))))

  )
