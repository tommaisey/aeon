(library (subdivide)
  (export in+impl in±impl to!impl)
  (import (scheme) (utilities) (event) (context) (node-eval))

  (define rest-symbol '~)
  (define :sustain ':sustain)

  ;; Implements the recursive subdivision of an input pdef into equal-sized
  ;; elements. An add-fn is supplied, which returns a list of events for each
  ;; leaf in the pdef.
  (define (papply context pdur pdef add-fn)
    (cond
     ((null? pdef) '())
     ((not (unsafe-list? pdef)) (papply context pdur (list pdef) add-fn))
     (else
      (let ([subdur (/ pdur (length pdef))]
	    [start (round-down-f (context-start context) pdur)])
	(let loop ([t start] [p pdef] [out '()])
	  (cond
	   ((null? p) (loop t pdef out))
	   ((>= t (context-end context)) out)
	   (else
	    (let* ([item (car p)]
		   [next-t (+ t subdur)]
		   [context (rearc context (make-arc t next-t))]
		   [value (if (unsafe-list? item)
			      (papply context subdur item add-fn)
			      (add-fn context t item))])
	      (loop next-t (cdr p) (append value out))))))))))

  ;; Adds blank events to the context with a subdividing pattern (pdef)
  ;; A pdef value of 1 gives one event.
  ;; For values > 1, creates N subdivided values.
  ;; The symbol ~ creates a rest.
  (define (in+impl context pdur pdef)
    (define (add-fn context t leaf)
      (in-adder
       (lambda (val context)
	 (let* ([num (max 1 val)]
		[dur (/ (context-length context) num)]
		[make (lambda (i) (make-event (+ t (* i dur)) (:sustain dur)))])
	   (map make (reverse (iota num)))))
       context t leaf))
    (context-trim
     (context-with-events
      context (reverse (papply context pdur pdef add-fn)))))

  ;; Adds events with a certain property filled in by a subdivding pattern.
  (define (in±impl key context pdur pdef)
    (define (add-fn context t leaf)
      (in-adder
       (lambda (val context)
	 (let ([dur (context-length context)])
	   (list (event-set (make-event t (:sustain dur)) key val))))
       context t leaf))
    (context-trim
     (context-with-events
      context (reverse (papply context pdur pdef add-fn)))))

  ;; Used by the 'in' impls above to get a value from a leaf node,
  ;; and to use it to add a new note/notes to the context.
  (define (in-adder maker context t leaf)
    (let ([val (get-leaf-early leaf t context)])
	(cond
	 ((or (eq? val rest-symbol) (eq? val 0))
	  '())
	 ((context? val)
	  (reverse (context-events-next val)))
	 ((not (number? val))
	  (raise (format "error in 'in' pattern - not a number, got: ~A" val)))
	 (else (maker val context)))))

  ;; Takes a pdef template and a context, and returns a new context with the
  ;; values in the pdef applied to any events in the context.
  (define (to!impl context pdur pdef key)
    (define (add-fn context t leaf)
      (let ([context (context-trim context)]
	    [morpher (lambda (c) (event-set (context-event c) key (get-leaf leaf c)))])
	(if (eq? leaf rest-symbol)
	    (context-events-next context)
	    (context-events-next (context-map morpher context)))))
    (context-with-events
     context (reverse (papply context pdur pdef add-fn))))
  
  )
