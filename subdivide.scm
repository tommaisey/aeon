#!chezscheme ;; Needed for the extra symbols like »

(library (subdivide)
  (export /- dispatch-pdef
	  rp:impl
	  in*impl
	  in:impl
	  to:impl
	  to-math-impl
	  mv-math-impl
	  echo-impl)
  
  (import (scheme)
	  (utilities)
	  (event)
	  (context)
	  (node-eval)
	  (for (pdef) expand))

  (define :sustain ':sustain)

  (define (pattern-error name type val)
    (format "pattern error: got '~A', ~A expects a ~A" val name type))

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
       (make-pdef dur (make-pdef-data def) subdiv-chunker))))

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
	  (raise (pattern-error "'rp'" "procedure" result))
	  (context-events-next result))))

  ;; Helper for 'in' impls to get a value from a leaf node,
  ;; and to use it to add a new note/notes to the context.
  (define (make-events maker context leaf)
    (let ([val (get-leaf-early leaf (context-start context) context)])
      (cond
       ((is-rest? val) '())
       ((context? val) (context-events-next val))
       ((not (number? val)) (raise (pattern-error "'in'" "number" val)))
       (else (maker val context)))))

  ;; Adds blank events to the context with a subdividing pattern.
  ;; A leaf value of 1 gives one event.
  ;; For values > 1, creates N subdivided values.
  ;; The symbol ~ creates a rest.
  (define (in*impl context leaf)
    (make-events
     (lambda (val context)
       (let* ([num (max 1 val)]
	      [dur (/ (context-length context) num)]
	      [start (context-start context)]
	      [make (lambda (i) (make-event (+ start (* i dur)) (:sustain dur)))])
	 (map make (iota num))))
     context leaf))

  ;; Adds events with a property defined by 'key'.
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
  
  ;; Sets the property 'key' on all events in the context.
  (define (to:impl key)
    (lambda (context leaf)
      (define (map-fn c)
	(set-or-rest c leaf key (lambda (v) v)))
      (context-events-next (context-map map-fn (context-resolve context)))))

  ;; Sets the property 'key' by doing some math on the old
  ;; value together with leaf.
  (define (to-math-impl math-fn key)
    (lambda (context leaf)
      (define (map-fn c)
	(let ([current (event-get (context-event c) key #f)])
	  (if (not current)
	      (context-event c)
	      (set-or-rest c leaf key (lambda (v) (math-fn current v))))))
      (context-events-next (context-map map-fn (context-resolve context)))))

  ;; Resolves input context with an arc shifted by leaf,
  ;; effectively moving a different slice of time into this one.
  (define (mv-math-impl math-fn inv-math-fn)
    (define (mapper new-val)
      (lambda (context)
	(event-move (context-event context) new-val math-fn)))
    (lambda (context leaf)
      (derecord context ([old-start context-start]
			 [old-end context-end])
	(context-events-next
	 (let ([val (get-leaf-early leaf old-start context)])
	   (cond
	    ((is-rest? val) (context-resolve context))
	    ((not (number? val)) (raise (pattern-error "'mv'" "number" val)))
	    (else
	     (let* ([new-start (inv-math-fn old-start val)]
		    [new-end (inv-math-fn old-end val)]
		    [shifted (rearc context (make-arc new-start new-end))])
	       (context-map (mapper val) (context-resolve shifted))))))))))

  (define (echo-impl num-taps iterative-nodes)
    (lambda (context division-leaf)
      '()))

  ;;-----------------------------------------------------------------------
  ;; General helpers for main time-chunking routines like subdiv-chunker.

  ;; Some helpers for dealing with repeat, rest and sustain symbols.
  (define (maybe-repeat next last)
    (if (eq? repeat-sym next) last next))

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

  ;; -> (values def-len, start-beat, slice-dur)
  (define (pdef-info def dur context)
    (let* ([len (length def)]
	   [slice-dur (/ dur len)]
	   [start (round-down-f (context-start context) dur)])
      (values len start slice-dur)))

  ;;-----------------------------------------------------------------------
  ;; Implements the recursive subdivision of an input pdef list into equal-sized
  ;; elements. A perform fn is supplied, which returns a list of events for each
  ;; leaf in the pdef list.
  ;; -> (list event ...)
  (define (subdiv-chunker context dur def perform)
    (cond
     ((null? def)
      '())
     ((not (unsafe-list? def))
      (subdiv-chunker context dur (list def) perform))
     (else
      (let-values (([len start slice-dur] (pdef-info def dur context)))
	(let loop ([t start]
		   [next def]
		   [prev #f]
		   [out '()])
	  (cond
	   ((null? next) (loop t def #f out))
	   ((>= t (context-end context)) out)
	   (else
	    (let-values ([[num-slices next-p] (drop-stretched next)])		   
	      (let* ([item (maybe-repeat (car next) prev)]
		     [next-t (+ t (* num-slices slice-dur))]
		     [context (rearc context (make-arc t next-t))]
		     [new (if (unsafe-list? item)
			      (subdiv-chunker context slice-dur item perform)
			      (perform context item))])
		(loop next-t next-p item (append out new)))))))))))
  
  )
