
(library (stretch)
  (export stretch pfill-notes)
  (import (scheme) (utilities) (note) (context))

  (define rest-symbol '~)

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

  ;; Returns a list of x repeated n times.
  ;;
  ;; (repeat 3 5) => (5 5 5)
  (define (repeat n x)
    (let loop ([n n] [o '()])
      (if (= 0 n) o (loop (- n 1) (cons x o)))))

  ;; Implements the recursive subdivision of an input pdef template into
  ;; equal-sized elements. Creation of the elements is left to an add-fn, which
  ;; should return a list of elements for an inpit value. It is also handed a
  ;; context, because pdef template elements are allowed to be c-vals. 
  (define (pfill context pdur pdef add-fn)
    (cond
     ((null? pdef) '())
     ((not (list? pdef)) (pfill context pdur (list pdef) add-fn))
     (else
      (let ([subdiv (/ pdur (length pdef))])
	(let loop ([t (* pdur (exact (truncate (/ (context-start context) pdur))))]
		   [lst pdef]
		   [out '()])
	  (cond
	   ((>= t (context-end context)) out)
	   ((null? lst) (loop t pdef out))
	   (else
	    (let* ([item (car lst)]
		   [next-t (+ t subdiv)]
		   [context (context-with-range context (make-range t next-t))]
		   [value (if (list? item)
			      (pfill context subdiv item add-fn)
			      (add-fn context item t subdiv))])
	      (loop next-t (cdr lst) (append value out))))))))))

  ;; Implements transformation of a pdef template into a list of notes.
  ;; A pdef value of 1 gives one note. For values > 1, creates N subdivided
  ;; notes. Returns () for a rest.
  (define (pfill-notes context pdur pdef)
    (define (add-fn sub-ctxt c-val t dur)
      (let ([val (contextualize-for-add c-val t sub-ctxt)])
	(cond
	 ((number? val)
	  (let* ([val (max 1 val)]
		 [dur (/ dur val)]
		 [make (lambda (i) (make-note (+ t (* i dur)) ('length dur)))])
	    (map make (reverse (iota val)))))
	 ((eq? val rest-symbol) '())
	 (else (raise "Event stretch patterns should only contain numbers.")))))
    (pfill context pdur pdef add-fn))

  ;; TODO: this is all wrong so far...
  (define-syntax event-pdef
    (syntax-rules ()
      ((_ pdef sdef)
       (lambda (context)
	 (let ([c (sdef-restrict-context sdef context)]
	       [d (sdef-gur sdef)]))
	 (pfill-notes c (auto-quasi pdef) d)))))
  
  (define-syntax stretch
    (syntax-rules (event)
      ((_) (lambda (context) context)) ;; Base case
      
      ((_ [event pdef sdef] rest ...)
       ((stretch rest) ... ((event-pdef pdef sdef) context)))))
  )
