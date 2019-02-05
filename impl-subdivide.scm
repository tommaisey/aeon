(library (impl-subdivide)
  (export pcreate pmorph)
  (import (scheme) (utilities) (note) (context))

  (define rest-symbol '~)

  ;; Returns a list of x repeated n times.
  ;;
  ;; (repeat 3 5) => (5 5 5)
  (define (repeat n x)
    (let loop ([n n] [o '()])
      (if (= 0 n) o (loop (- n 1) (cons x o)))))

  ;; Implements the recursive subdivision of an input pdef template into
  ;; equal-sized elements. Creation of the elements is left to an add-fn, which
  ;; should return a list of elements for an input value. It is also handed a
  ;; context, because pdef template elements are allowed to be c-vals. 
  (define (papply context pdur pdef add-fn)
    (cond
     ((null? pdef) '())
     ((not (unsafe-list? pdef)) (papply context pdur (list pdef) add-fn))
     (else
      (let ([subdur (/ pdur (length pdef))]
	    [start (round-down-f (context-start context) pdur)])
	(let loop ([t start] [p pdef] [out '()])
	  (cond
	   ((>= t (context-end context)) out)
	   ((null? p) (loop t pdef out))
	   (else
	    (let* ([item (car p)]
		   [next-t (+ t subdur)]
		   [context (rerange context (make-range t next-t))]
		   [value (if (unsafe-list? item)
			      (papply context subdur item add-fn)
			      (add-fn context t item))])
	      (loop next-t (cdr p) (append value out))))))))))

  ;; Implements transformation of a pdef template into a list of notes.
  ;; A pdef value of 1 gives one note. For values > 1, creates N subdivided
  ;; notes. The symbol ~ creates a rest. Notes are returned in reverse order.
  (define (pcreate context pdur pdef)
    (define (add-fn context t c-val)
      (let ([val (get-c-val c-val t context)])
	(cond
	 ((or (eq? val rest-symbol) (= val 0)) '())
	 ((number? val)
	  (let* ([num (max 1 val)]
		 [dur (/ (context-length context) num)]
		 [mke (lambda (i) (make-note (+ t (* i dur)) ('length dur)))])
	    (map mke (reverse (iota num)))))
	 (else (raise "Event stretch patterns should only contain numbers.")))))
    (context-with-notes
     context (reverse (papply context pdur pdef add-fn))))

  ;; Implements transformation of a pdef template into a context-transformer.
  ;; Events falling inside each subdivided time range of the pattern are given
  ;; the corresponding value at key.
  (define (pmorph context pdur pdef key)
    (define (add-fn context t c-val)
      (let* ([val (get-c-val c-val t context)]
	     [mke (lambda (n) (note-set n key val))]
	     [notes (context-notes-next (context-trim context))])
	(map mke notes)))
    (context-with-notes
     context (reverse (papply context pdur pdef add-fn))))

  )
