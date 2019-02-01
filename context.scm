;; -*- geiser-scheme-implementation: chez-*-
(library (context)
  (export
    context
    make-context
    context-note
    context-notes-next
    context-notes-prev
    context-range
    context-print
    context-move
    context-to-closest-note
    context-complete?
    context-rewind
    context-map
    context-filter
    contexts-merge)
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
	  (new notes-next '() range))))))

  (define (context-note c)
    (car (context-notes-next c)))

  (define (context-print c)
    (let ([win (context-range c)])
      (display "Range: ")
      (display (range-start win))
      (display ", ")
      (display (range-end win))
      (newline)
      (print-notes (context-notes-next c))))

  ;; pass a context and notes-next & notes-prev.
  ;; swap their order to move one item forward or back.
  ;; moves the head of list returned by (get-next c)
  (define (context-move1 c get-next get-prev)
    (let ([next (get-next c)] [prev (get-prev c)])
      (make-context (if (pair? next) (cdr next) '())
		    (if (pair? prev) (cons (car next) prev) '())
		    (context-range c))))

  (define (context-move1-fwd c)
    (context-move1 c context-notes-next context-notes-prev))
  (define (context-move1-bck c)
    (context-move1 c context-notes-prev context-notes-next))

  (define (context-traverse c direction-fn)
    (let* ([dir (direction-fn c)]
	   [mv (if (positive? dir) context-move1-fwd context-move1-bck)])
      (if (zero? dir) c (context-traverse (mv c) direction-fn))))

  ;; direction-fn that drives context iteration forward/back n times,
  ;; depending on whether n is positive or negative, or until the next/prev
  ;; list runs out.
  (define (until-zero-or-end n)
    (lambda (context)
      (let ([x n]
	    [get (if (positive? n) context-notes-next context-notes-prev)])
	(cond
	 ((or (zero? x) (null? (get context))) 0)
	 ((positive? x) (set! n (sub1 n)) x)
	 ((negative? x) (set! n (add1 n)) x)))))

  ;; direction-fn that moves a context iterator to the note
  ;; in the context that starts closest to the requested time. 
  (define (to-closest time)
    (lambda (context)
      (define (delta note) (- time (note-get note time-key +inf.0)))
      (define (car-delta l) (if (null? l) +inf.0 (delta (car l))))
      (define (cdr-delta l) (if (null? l) +inf.0 (car-delta (cdr l))))
      (let* ([cur (delta (context-note context))]
	     [prv (car-delta (context-notes-prev context))]
	     [nxt (cdr-delta (context-notes-next context))])
	(if (<= (abs cur) (abs nxt))
	    (if (< (abs prv) (abs cur)) -1 0) 1))))

  (define (context-move c n)
    (context-traverse c (until-zero-or-end n)))

  (define (context-to-closest-note c time)
    (context-traverse c (to-closest time)))

  (define (context-complete? c)
    (eqv? '() (cdr (context-notes-next c))))
  
  (define (context-rewind c)
    (context-move c -9999999)) ;; Lazy

  ;; Used in context-map and context-filter below
  (define (context-transform context build-notes-fn)
    (let recur ([c context] [output '()])
      (if (context-complete? c)
	  (make-context (reverse output) '() (context-range c))
	  (recur (context-move1-fwd c)
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

  ) ; end module context
