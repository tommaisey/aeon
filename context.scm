;; -*- geiser-scheme-implementation: chez-*-
(library (context)
  (export
    context
    make-context
    make-empty-context
    context-event
    context-events-next
    context-events-prev
    context-range
    context-start
    context-end
    context-now
    context-print
    context-with-events
    context-clear-events
    context-with-range rerange
    context-length
    context-move
    context-to-closest-event
    context-to-event-after
    context-rewind
    context-map
    context-filter
    context-trim
    context-empty?
    contexts-merge
    get-c-val)
  (import (chezscheme) (utilities) (event))

  ;; ---------------------------------------------
  ;; A context for a event in a series. events-next contains the
  ;; remaining events (starting with 'this' event) and prev-events
  ;; contains the previous events in reverse, starting with the one
  ;; preceding 'this'.
  (define-record-type context
    (fields (immutable events-next)
	    (immutable events-prev)
	    (immutable range))
    (protocol
     (lambda (new)
       (case-lambda ; events-prev is optional in ctor
	 ((events-next events-prev range)
	  (new events-next events-prev range))
	 ((events-next range)
	  (new events-next '() range))
	 ((range)
	  (new '() '() range))))))

  (define (make-empty-context start end)
    (make-context (make-range start end)))

  (define (context-event c)
    (let ([n (context-events-next c)])
      (if (null? n) '() (car n))))

  (define (context-start c) (range-start (context-range c)))
  (define (context-end c) (range-end (context-range c)))

  (define (context-now c)
    (let ([e (context-event c)])
      (if (null? e)
	  (context-start c)
	  (event-beat e))))

  ;; For use as a record-writer in chez (see init.scm)
  (define (context-print c port wr)
    (display "Range: " port)
    (display (context-start c) port)
    (display ", " port)
    (display (context-end c) port)
    (newline port)
    (print-events (context-events-next c) port))

  (define context-with-events
    (case-lambda
      ((c nxt) (context-with-events c nxt '()))
      ((c nxt prv) (make-context nxt prv (context-range c)))))

  (define (context-clear-events c)
    (context-with-events c '()))

  (define (context-with-range c r)
    (make-context (context-events-next c) (context-events-prev c) r))
  (define rerange context-with-range)

  (define (context-length c)
    (range-length (context-range c)))

  ;;--------------------------------------------------------------------
  ;; Iteration. A context has a list of previous and next events - these
  ;; functions assist in moving around in the context, as well as things like
  ;; mapping, filtering etc.

  ;; Public iteration functions
  (define (context-rewind c)
    (cond
     ((context-empty? c) c)
     ((context-last? c)
      (context-with-events c (reverse (cons (context-event c) (context-events-prev c)))))
     (else (context-move c -9999999)))) ;; Lazy
  
  (define (context-move c n)
    (context-it c (until-zero-or-end n)))

  (define (context-to-closest-event c time)
    (context-it c (to-closest time)))

  (define (context-to-event-after c time)
    (context-it c (to-before time)))

  ;;----------------------------------------------------------------------
  ;; Transformations.
  ;;
  ;; Takes a lambda taking a context, returning a event.
  (define (context-map new-event-fn context)
    (context-transform
     context (lambda (c output) (cons (new-event-fn c) output))))

  ;; Takes a lambda taking a context, returning bool.
  (define (context-filter pred context)
    (context-transform
     context (lambda (c output)
	       (if (pred c) (cons (context-event c) output) output))))

  ;; Removes events from the context that don't fall within range.
  (define (context-trim context)
    (define (pred c)
      (between (event-beat (context-event c)) (context-start c) (context-end c)))
    (context-filter pred context))

  (define (contexts-merge c1 c2)
    (let ([c1 (context-rewind c1)]
	  [c2 (context-rewind c2)])
      (make-context (merge-sorted (context-events-next c1)
				  (context-events-next c2)
				  event-before?)
		    (make-range (min (context-start c1) (context-start c2))
				(max (context-end c1) (context-end c2))))))

  ;;------------------------------------------------------------------------
  ;; See c-val.scm for details. This is kept here because it frees other
  ;; libraries from importing (c-vals).
  (define get-c-val
    (case-lambda
      ((c-val context)
       (if (procedure? c-val)
	   (c-val context) c-val))

      ;; If we use a c-val when adding a new event, the context will look wrong.
      ;; The event doesn't yet exist, so e.g. next/prev functions would be broken.
      ;; In this case, add an empty event to the context before evaluating.
      ((c-val time-to-add context)
       (if (procedure? c-val)
	   (c-val (context-insert context (make-event time-to-add)))
	   c-val))))

  ;;---------------------------------------------------------------------
  ;; Helpers used in public iteration functions.
  
  ;; Swap the order of next/prev to move one item forward or back.
  (define (context-move1 c get-next get-prev)
    (let ([next (get-next c)] [prev (get-prev c)])
      (if (null? next) c
	  (context-with-events c (cdr next) (cons (car next) prev)))))
  (define (context-move1-fwd c)
    (context-move1 c context-events-next context-events-prev))
  (define (context-move1-bck c)
    (context-move1 c context-events-prev context-events-next))

  ;; direction-fn returns +1 for move fwd, -1 for back, 0 for stop.
  (define (context-it c direction-fn)
    (let* ([dir (direction-fn c)]
	   [mv (if (positive? dir) context-move1-fwd context-move1-bck)])
      (if (zero? dir) c (context-it (mv c) direction-fn))))

  ;; direction-fn that drives context iteration forward/back n times,
  ;; for positive/negative n, or until the next/prev list runs out.
  (define (until-zero-or-end n)
    (lambda (context)
      (let ([x n]
	    [get (if (positive? n)
		     context-events-next
		     context-events-prev)])
	(cond
	 ((or (zero? x) (null? (get context))) 0)
	 ((positive? x) (set! n (sub1 n)) x)
	 ((negative? x) (set! n (add1 n)) x)))))

  ;; direction-fn that drives context iteration to the event closest
  ;; to the requested time. 
  (define (to-closest time)
    (lambda (c)
      (define (get-delt bounds-check? default get-event)
	(if (bounds-check? c) default (delta time (get-event c))))
      (let ([cur (get-delt context-empty? 0 context-event)]
	    [prv (get-delt context-first? -inf.0 context-prev-unchecked)]
	    [nxt (get-delt context-last?  +inf.0 context-next-unchecked)])
	(if (<= (abs cur) (abs nxt))
	    (if (< (abs prv) (abs cur)) -1 0) +1))))

  ;; direction-fn that moves a context to the first event that's greater
  ;; than or equal to the requested time.
  (define (to-before time)
    (lambda (c)
      (define (get-delt bounds-check? default get-event)
	(if (bounds-check? c) default (delta time (get-event c))))
      (let ([cur (get-delt context-empty? 0 context-event)]
	    [prv (get-delt context-first? -inf.0 context-prev-unchecked)]
	    [nxt (get-delt context-last?  +inf.0 context-next-unchecked)])
	(cond
	 ((and (> cur 0) (>= nxt 0) (< nxt cur)) +1)
	 ((and (< cur 0) (<= prv 0) (> prv cur)) -1)
	 (else 0)))))

  (define (context-first? c)
    (null? (context-events-prev c)))

  (define (context-last? c)
    (or (context-empty? c)
	(null? (cdr (context-events-next c)))))

  (define (context-it-end? c)
    (null? (context-events-next c)))

  (define (context-empty? c)
    (and (null? (context-events-next c))
	 (null? (context-events-prev c))))

  (define (context-next-unchecked c) (cadr (context-events-next c)))
  (define (context-prev-unchecked c) (car (context-events-prev c)))
  (define (delta t event) (- t (event-get event time-key +inf.0)))

  ;; Used in context-map and context-filter
  (define (context-transform context build-events-fn)
    (let loop ([c context]
	       [output '()])
      (if (context-it-end? c)
	  (context-with-events c (reverse output))
	  (loop (context-move1-fwd c)
		(build-events-fn c output)))))

  ;; Inserts the new event in a sorted fashion into the context, leaving the
  ;; context pointing to the new event, not the original one.
  (define (context-insert c new-event)
    (if (context-empty? c)
	(context-with-events c (list new-event))
	(let* ([moved (context-to-event-after c (event-beat new-event))]
	       [nxt (context-events-next moved)]
	       [prv (context-events-prev moved)])
	  (context-with-events c (cons new-event nxt) prv))))

  ) ; end module context
