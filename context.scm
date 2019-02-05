;; -*- geiser-scheme-implementation: chez-*-
(library (context)
  (export
    context
    make-context
    context-note
    context-notes-next
    context-notes-prev
    context-range
    context-start
    context-end
    context-print
    context-with-notes
    context-with-range
    context-move
    context-to-closest-note
    context-to-note-after
    context-rewind
    context-map
    context-filter
    contexts-merge
    contextualize
    contextualize-for-add)
  (import (chezscheme) (utilities) (note))

  ;; ---------------------------------------------
  ;; A context for a note in a series. notes-next contains the
  ;; remaining notes (starting with 'this' note) and prev-notes
  ;; contains the previous notes in reverse, starting with the one
  ;; preceding 'this'.
  (define-record-type context
    (fields (immutable notes-next)
	    (immutable notes-prev)
	    (immutable range))
    (protocol
     (lambda (new)
       (case-lambda ; notes-prev is optional in ctor
	 ((notes-next notes-prev range)
	  (new notes-next notes-prev range))
	 ((notes-next range)
	  (new notes-next '() range))
	 ((range)
	  (new '() '() range))))))

  (define (context-note c)
    (let ([n (context-notes-next c)])
      (if (null? n) '() (car n))))

  (define (context-start c) (range-start (context-range c)))
  (define (context-end c) (range-end (context-range c)))

  (define (context-print c)
    (display "Range: ")
    (display (context-start c))
    (display ", ")
    (display (context-end c))
    (newline)
    (print-notes (context-notes-next c)))

  (define context-with-notes
    (case-lambda
      ((c nxt) (context-with-notes c nxt '()))
      ((c nxt prv) (make-context nxt prv (context-range c)))))

  (define (context-with-range c r)
    (make-context (context-notes-next c) (context-notes-prev c) r))

  ;;--------------------------------------------------------------------
  ;; Iteration. A context has a list of previous and next notes - these
  ;; functions assist in moving around in the context, as well as things like
  ;; mapping, filtering etc.

  ;; Public iteration functions
  (define (context-rewind c)
    (if (context-ended? c)
	(context-with-notes c (reverse (cons (context-note c) (context-notes-prev c))))
	(context-move c -9999999))) ;; Lazy
  
  (define (context-move c n)
    (context-it c (until-zero-or-end n)))

  (define (context-to-closest-note c time)
    (context-it c (to-closest time)))

  (define (context-to-note-after c time)
    (context-it c (to-before time)))

  ;;---------------------------------------------------------------------
  ;; Helpers used in public iteration functions.
  
  ;; Swap the order of next/prev to move one item forward or back.
  (define (context-move1 c get-next get-prev)
    (let ([next (get-next c)] [prev (get-prev c)])
      (if (null? next) c
	  (context-with-notes c (cdr next) (cons (car next) prev)))))
  (define (context-move1-fwd c)
    (context-move1 c context-notes-next context-notes-prev))
  (define (context-move1-bck c)
    (context-move1 c context-notes-prev context-notes-next))

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
		     context-notes-next
		     context-notes-prev)])
	(cond
	 ((or (zero? x) (null? (get context))) 0)
	 ((positive? x) (set! n (sub1 n)) x)
	 ((negative? x) (set! n (add1 n)) x)))))

  ;; direction-fn that drives context iteration to the note closest
  ;; to the requested time. 
  (define (to-closest time)
    (lambda (c)
      (define (get-delt bounds-check? default get-note)
	(if (bounds-check? c) default (delta time (get-note c))))
      (let ([cur (get-delt context-empty? 0 context-note)]
	    [prv (get-delt context-start? -inf.0 context-prev-unchecked)]
	    [nxt (get-delt context-ended? +inf.0 context-next-unchecked)])
	(if (<= (abs cur) (abs nxt))
	    (if (< (abs prv) (abs cur)) -1 0) +1))))

  ;; direction-fn that moves a context to the first note that's greater
  ;; than or equal to the requested time.
  (define (to-before time)
    (lambda (c)
      (define (get-delt bounds-check? default get-note)
	(if (bounds-check? c) default (delta time (get-note c))))
      (let ([cur (get-delt context-empty? 0 context-note)]
	    [prv (get-delt context-start? -inf.0 context-prev-unchecked)]
	    [nxt (get-delt context-ended? +inf.0 context-next-unchecked)])
	(cond
	 ((and (> cur 0) (>= nxt 0) (< nxt cur)) +1)
	 ((and (< cur 0) (<= prv 0) (> prv cur)) -1)
	 (else 0)))))

  (define (context-start? c)
    (eqv? '() (context-notes-prev c)))
  
  (define (context-ended? c)
    (eqv? '() (cdr (context-notes-next c))))

  (define (context-empty? c)
    (and (null? (context-notes-next c))
	 (null? (context-notes-prev c))))

  (define (context-next-unchecked c) (cadr (context-notes-next c)))
  (define (context-prev-unchecked c) (car (context-notes-prev c)))
  (define (delta t note) (- t (note-get note time-key +inf.0)))

  ;; Used in context-map and context-filter below
  (define (context-transform context build-notes-fn)
    (let loop ([c context]
	       [output '()])
      (if (context-ended? c)
	  (context-with-notes c (reverse output))
	  (loop (context-move1-fwd c)
		(build-notes-fn c output)))))

  ;; Takes a lambda taking a context, returning a note
  (define (context-map new-note-fn context)
    (context-transform
     context (lambda (c output) (cons (new-note-fn c) output))))

  ;; Takes a lambda taking a context, returning bool
  (define (context-filter pred context)
    (context-transform
     context (lambda (c output)
	       (if (pred c) (cons (context-note c) output) output))))

  (define (contexts-merge c1 c2)
    (let ([c1 (context-rewind c1)]
	  [c2 (context-rewind c2)])
      (make-context (merge-sorted (context-notes-next c1)
				  (context-notes-next c2)
				  note-before?)
		    (make-range (min (range-start (context-range c1))
				     (range-start (context-range c2)))
				(max (range-end   (context-range c1))
				     (range-end   (context-range c2)))))))

  ;; Inserts the new note in a sorted fashion into the context, leaving the
  ;; context pointing to the new note, not the original one.
  (define (context-insert c new-note)
    (if (context-empty? c)
	(context-with-notes c (list new-note))
	(let* ([moved (context-to-note-after c (note-beat new-note))]
	       [nxt (context-notes-next moved)]
	       [prv (context-notes-prev moved)])
	  (context-with-notes c (cons new-note nxt) prv))))

  ;; We want our patterns to allow both simple values and 'contextual' values,
  ;; which are functions taking a context and returning a value. The sum of these
  ;; two types is known as a 'c-val'. This function gets the value of a c-val.
  (define (contextualize c-val context)
    (if (procedure? c-val) (c-val context) c-val))

  ;; We want to allow c-vals (see above) when creating notes too, but in this
  ;; case there's no note for the contextual value to refer to yet (it's about
  ;; to be created). This adds a fake note at the right time, so the context makes sense.
  (define (contextualize-for-add c-val t context)
    (if (procedure? c-val)
	(c-val (context-insert context (make-note t)))
	c-val))

  ) ; end module context
