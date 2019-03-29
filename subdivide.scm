#!chezscheme ;; Needed for the extra symbols like »

(library (subdivide)
  (export /- dispatch-pdef
	  in*impl
	  in:impl
	  to:impl
	  to-math-impl
	  rp:impl)
  
  (import (scheme)
	  (utilities)
	  (event)
	  (context)
	  (node-eval)
	  (for (pdef) expand))

  (define :sustain ':sustain)

  (define pattern-error-in "pattern error: got '~A', in: expects a number")
  (define pattern-error-rp "pattern error: got '~A', rp: expects a procedure")

  ;;-------------------------------------------------------------------
  (define-record-type pdef
    (fields
     dur  ;; Length of the pattern represented here, in beats
     data ;; List of values/procedures created by make-pdef-data

     ;; A procedure that interprets a pdef as a series of time chunks,
     ;; and calls a 'performing' function for each of those chunks with
     ;; the corresponding value.
     time-chunker))

  (define-syntax /-
    (syntax-rules ()
      ((_ def) (/- 1 def))

      ((_ dur def)
       (make-pdef dur (make-pdef-data def) subdiv))))

  ;; Executes the time-chunker of a pdef, which will call the
  ;; perform-fn for each chunk of the context as it sees fit.
  (define (dispatch-pdef def context perform-fn)
    (let ([def (if (pdef? def) def (/- def))])
      (derecord def ([fn  pdef-time-chunker]
		     [dur pdef-dur]
		     [def pdef-data])
		(let ([events (fn context dur def perform-fn)])
		  (context-trim (context-with-events context events))))))

  ;;-----------------------------------------------------------------------
  ;; Implementations for specific ops. These don't have a concept of how to
  ;; apply themselves to different time regions - that's handled by the
  ;; pdef-time-chunker.

  (define (rp:impl context leaf)
    (let ([result (get-leaf leaf context)])
      (if (not (context? result))
	  (raise (format pattern-error-rp result))
	  (context-events-next result))))

  ;; Helper for 'in' impls to get a value from a leaf node,
  ;; and to use it to add a new note/notes to the context.
  (define (make-events maker context leaf)
    (let ([val (get-leaf-early leaf (context-start context) context)])
      (cond
       ((is-rest? val) '())
       ((context? val) (context-events-next val))
       ((not (number? val)) (raise (format pattern-error-in val)))
       (else (maker val context)))))

  ;; Adds blank events to the context with a subdividing pattern (pdef)
  ;; A pdef value of 1 gives one event.
  ;; For values > 1, creates N subdivided values.
  ;; The symbol ~ creates a rest.
  (define (in*impl context leaf)
    (make-events
     (lambda (val context)
       (let* ([num (max 1 val)]
	      [dur (/ (context-length context) num)]
	      [start (context-start context)])
	 (define (make i) (make-event (+ start (* i dur)) (:sustain dur)))
	 (map make (iota num))))
     context leaf))

  ;; Adds events with a certain property
  (define (in:impl key)
    (lambda (context leaf)
      (make-events
       (lambda (val context)
	 (let ([dur (context-length context)]
	       [start (context-start context)])
	   (list (event-set (make-event start (:sustain dur)) key val))))
       context leaf)))

  ;; Helper for 'to' forms below.
  (define (set-or-rest c leaf key val-transform)
    (let ([val (get-leaf leaf c)])
      (if (is-rest? val)
	  (context-event c)
	  (event-set (context-event c) key (val-transform val)))))

  ;; Takes a pdef template and a context, and returns a new context with the
  ;; values in the pdef applied to any events in the context.
  (define (to:impl key)
    (lambda (context leaf)
      (define (map-fn c)
	(set-or-rest c leaf key (lambda (v) v)))
      (context-events-next (context-map map-fn (context-trim context)))))
  
  (define (to-math-impl math-fn key)
    (lambda (context leaf)
      (define (map-fn c)
	(let ([current (event-get (context-event c) key #f)])
	  (if (not current)
	      (context-event c)
	      (set-or-rest c leaf key (lambda (v) (math-fn current v))))))
      (context-events-next (context-map map-fn (context-trim context)))))

  ;;-----------------------------------------------------------------------
  ;; General helpers for main time-chunking routines like subdiv (which -/
  ;; implements the /- pattern style).
  
  ;; Evaluates any splicers to build the final pdef, plus extra info:
  ;; -> (values expanded-def, def-len, start-beat, slice-dur)
  (define (splice-expand def dur context)
    (let* ([basic-len (length def)]
	   [basic-dur (/ dur basic-len)]
	   [start (round-down-f (context-start context) dur)])
      (let loop ([p def] [out '()] [len 0])
	(if (null? p)
	    (values (reverse out) len start (/ dur len))
	    (let* ([v (car p)]
		   [end (+ start (* len basic-dur))]
		   [x (if (splicer? v)
			  (reverse ((splicer-logic v) (make-empty-context start end)))
			  (list v))])
	      (loop (cdr p) (append x out) (+ len (length x))))))))

  ;; Some helpers for dealing with repeat, rest and sustain symbols.
  (define (maybe-repeat item last)
    (if (eq? repeat-sym item) last item))

  (define (is-rest? item)
    (eq? item rest-sym))

  (define (is-sustain? item)
    (eq? item sustain-sym))

  ;; Drops at least one value, more if the following values are sustains.
  ;; -> (values num-dropped new-lst)
  (define (drop-stretched lst)
    (let loop ([lst lst] [n 0])
      (if (and (not (null? lst))
	       (or (zero? n)
		   (is-sustain? (car lst))))
	  (loop (cdr lst) (+ n 1))
	  (values n lst))))

  ;;-----------------------------------------------------------------------
  ;; Implements the recursive subdivision of an input pdef list into equal-sized
  ;; elements. A perform fn is supplied, which returns a list of events for each
  ;; leaf in the pdef list.
  ;; -> (list event ...)
  (define (subdiv context dur def perform)
    (cond
     ((null? def) '())
     ((not (unsafe-list? def)) (subdiv context dur (list def) perform))
     (else
      (let-values ([[def len start dur]
		    (splice-expand def dur context)])
	(let loop ([t start] [p def] [last #f] [out '()])
	  (cond
	   ((null? p) (loop t def #f out))
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
  
  )
