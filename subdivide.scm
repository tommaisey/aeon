#!chezscheme ;; Needed for the extra symbols like »

(library (subdivide)
  (export in*impl in:impl to:impl to-math-impl)
  (import (scheme) (utilities) (event) (context) (node-eval) (pdef))

  (define :sustain ':sustain)

  ;; Evaluates any splicers to build the final pdef, plus extra info:
  ;; -> (values expanded-pdef, pdef-len, start-beat, slice-dur)
  (define (pdef-splice-expand pdef pdur context)
    (let* ([basic-len (length pdef)]
	   [basic-dur (/ pdur basic-len)]
	   [start (round-down-f (context-start context) pdur)])
      (let loop ([p pdef] [out '()] [len 0])
	(if (null? p)
	    (values (reverse out) len start (/ pdur len))
	    (let* ([v (car p)]
		   [end (+ start (* len basic-dur))]
		   [x (if (splicer? v)
			  (reverse ((splicer-logic v) (make-empty-context start end)))
			  (list v))])
	      (loop (cdr p) (append x out) (+ len (length x))))))))

  ;; Drops at least one value, more if the following values are sustains.
  ;; -> (values num-dropped new-lst)
  (define (drop-stretched lst)
    (let loop ([lst lst] [n 0])
      (if (and (not (null? lst))
	       (or (zero? n)
		   (eq? sustain-sym (car lst))))
	  (loop (cdr lst) (+ n 1))
	  (values n lst))))

  (define (maybe-repeat item last)
    (if (eq? repeat-sym item) last item))

  (define (is-rest? item)
    (eq? item rest-sym))

  ;; Implements the recursive subdivision of an input pdef into equal-sized
  ;; elements. An add-fn is supplied, which returns a list of events for each
  ;; leaf in the pdef.
  ;; -> (list note ...)
  (define (subdiv context pdur pdef perform)
    (cond
     ((null? pdef) '())
     ((not (unsafe-list? pdef)) (subdiv context pdur (list pdef) perform))
     (else
      (let-values ([[pdef len start dur]
		    (pdef-splice-expand pdef pdur context)])
	(let loop ([t start] [p pdef] [last #f] [out '()])
	  (cond
	   ((null? p) (loop t pdef #f out))
	   ((>= t (context-end context)) out)
	   (else
	    (let-values ([[stretch-num next-p] (drop-stretched p)])		    
	      (let* ([item (maybe-repeat (car p) last)]
		     [next-t (+ t (* stretch-num dur))]
		     [context (rearc context (make-arc t next-t))]
		     [new (if (unsafe-list? item)
			      (subdiv context dur item perform)
			      (perform context item))])
		(loop next-t next-p item (append out new)))))))))))

  ;; Just wraps the subdiv call, building a new context.
  (define (apply-subdiv-to-context context pdur pdef perform-fn)
    (context-trim
     (context-with-events
      context (subdiv context pdur pdef perform-fn))))

  ;; Helper for 'in' impls below to get a value from a leaf node,
  ;; and to use it to add a new note/notes to the context.
  (define (in-adder maker context leaf)
    (let ([val (get-leaf-early leaf (context-start context) context)])
      (cond
       ((is-rest? val) '())
       ((context? val)
	(context-events-next val))
       ((not (number? val))
	(raise (format "pattern error: got '~A', expecting a number" val)))
       (else (maker val context)))))

  ;; Adds blank events to the context with a subdividing pattern (pdef)
  ;; A pdef value of 1 gives one event.
  ;; For values > 1, creates N subdivided values.
  ;; The symbol ~ creates a rest.
  (define (in*impl context pdur pdef)
    (define (perform context leaf)
      (in-adder
       (lambda (val context)
	 (let* ([num (max 1 val)]
		[dur (/ (context-length context) num)]
		[start (context-start context)])
	   (define (make i) (make-event (+ start (* i dur)) (:sustain dur)))
	   (map make (iota num))))
       context leaf))
    (apply-subdiv-to-context context pdur pdef perform))

  ;; Adds events with a certain property filled in by a subdivding pattern.
  (define (in:impl key context pdur pdef)
    (define (perform context leaf)
      (in-adder
       (lambda (val context)
	 (let ([dur (context-length context)]
	       [start (context-start context)])
	   (list (event-set (make-event start (:sustain dur)) key val))))
       context leaf))
    (apply-subdiv-to-context context pdur pdef perform))

  ;; Helper for 'to' forms below.
  (define (set-or-rest c leaf key val-transform)
    (let ([val (get-leaf leaf c)])
      (if (is-rest? val)
	  (context-event c)
	  (event-set (context-event c) key (val-transform val)))))

  ;; Takes a pdef template and a context, and returns a new context with the
  ;; values in the pdef applied to any events in the context.
  (define (to:impl context pdur pdef key)
    (define (perform context leaf)
      (define (morpher c)
	(set-or-rest c leaf key (lambda (v) v)))
      (context-events-next (context-map morpher (context-trim context))))
    (apply-subdiv-to-context context pdur pdef perform))
  
  (define (to-math-impl math-fn context pdur pdef key)
    (define (perform context leaf)
      (define (morpher c)
	(let ([current (event-get (context-event c) key #f)])
	  (if (eqv? current #f)
	      (context-event c)
	      (set-or-rest c leaf key (lambda (v) (math-fn current v))))))
      (context-events-next (context-map morpher (context-trim context))))
    (apply-subdiv-to-context context pdur pdef perform))
  
  )
