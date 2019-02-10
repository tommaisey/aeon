(library (subdivide)
  (export pcreate pmorph)
  (import (scheme) (utilities) (event) (context))

  (define rest-symbol '~)

  ;; Implements the recursive subdivision of an input pdef into equal-sized
  ;; elements. An add-fn is supplied, which returns a list of events for each
  ;; c-val in the pdef.
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
		   [context (rerange context (make-range t next-t))]
		   [value (if (unsafe-list? item)
			      (papply context subdur item add-fn)
			      (add-fn context t item))])
	      (loop next-t (cdr p) (append value out))))))))))

  ;; Takes a pdef template and a context, and returns a new context with the
  ;; same range, containing events according to the the pdef template.
  ;; A pdef value of 1 gives one event. For values > 1, creates N subdivided
  ;; events. The symbol ~ creates a rest.
  (define (pcreate context pdur pdef)
    (define (add-fn context t c-val)
      (let ([val (get-c-val c-val t context)])
	(cond
	 ((or (eq? val rest-symbol) (eq? val 0))
	  (list))
	 ((not (number? val))
	  (raise (string-append "subdivide for 'event' should "
				"contain only numbers or rests.")))
	 (else
	  (let* ([num (max 1 val)]
		 [dur (/ (context-length context) num)]
		 [mke (lambda (i) (make-event (+ t (* i dur)) (:sustain dur)))])
	    (map mke (reverse (iota num))))))))
    (context-trim
     (context-with-events
      context (reverse (papply context pdur pdef add-fn)))))

  ;; Takes a pdef template and a context, and returns a new context with the
  ;; values in the pdef applied to any events in the context.
  (define (pmorph context pdur pdef key)
    (define (add-fn context t c-val)
      (let ([context (context-trim context)]
	    [morpher (lambda (c) (event-set (context-event c) key (get-c-val c-val c)))])
	(if (eq? c-val rest-symbol)
	    (context-events-next context)
	    (context-events-next (context-map morpher context)))))
    (context-with-events
     context (reverse (papply context pdur pdef add-fn))))

  )
