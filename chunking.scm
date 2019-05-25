#!chezscheme ;; Needed for extra symbols like »

;; Implements algorithms that slice a context up into chunks according
;; to several schemes, and call an implementation function for each
;; chunk. The implementation function must take a context (which will
;; have the required arc) and a 'leaf', which is either a value or a
;; function that returns a value.

(library (chunking)
  (export sbdv step dispatch-pdef
	  is-rest? is-sustain?
	  subdivider
	  pattern-error)
  
  (import (scheme)
	  (utilities)
	  (event)
	  (context)
	  (node-eval)
	  (for (pdef) expand))

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

  (define-syntax sbdv
    (syntax-rules ()
      ((_ def) (sbdv 1 def))

      ((_ dur def)
       (make-pdef dur (make-pdef-data def) subdivider))))

  (define-syntax step
    (syntax-rules ()
      ((_ def) (step 1/4 def))

      ((_ slice-dur def)
       (let* ([data (make-pdef-data def)])
	 (make-pdef (* slice-dur (length data)) data subdivider)))))

  ;; Executes the time-chunker of a pdef, which will call the
  ;; perform-fn for each chunk of the context as it sees fit.
  (define (dispatch-pdef def context perform-fn)
    (let ([def (if (pdef? def) def (sbdv def))])
      (derecord def ([fn  pdef-time-chunker]
		     [dur pdef-dur]
		     [def pdef-data])
	(let ([events (fn context dur def perform-fn)])
	  (context-trim (context-with-events context events))))))

  ;;-----------------------------------------------------------------------
  ;; General helpers for main time-chunking routines like subdivider.

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
  (define (subdivider context dur def perform)
    
    (define (build-events item subctxt slice-dur)
      (if (arcs-overlap? (context-arc context)
			 (context-arc subctxt))
	  (if (unsafe-list? item)
	      (subdivider subctxt slice-dur item perform)
	      (perform subctxt item))
	  (list)))
    (cond
     ((null? def) (list))
     ((not (unsafe-list? def))
      (subdivider context dur (list def) perform))
     (else
      (let-values (([len start slice-dur]
		    (pdef-info def dur context)))
	(let loop ([t start]
		   [next def]
		   [prev #f]
		   [events '()])
	  (cond
	   ((null? next) (loop t def #f events))
	   ((>= t (context-end context)) events)
	   (else
	    (let-values ([[num-slices next-p] (drop-stretched next)])		  
	      (let* ([item (maybe-repeat (car next) prev)]
		     [next-t (+ t (* num-slices slice-dur))]
		     [subctxt (rearc context (make-arc t next-t))]
		     [new-events (build-events item subctxt slice-dur)])
		(loop next-t next-p item (append events new-events)))))))))))
  
  )
