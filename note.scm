;; -*- geiser-scheme-implementation: chez-*-
;;-------------------------------------------------------------
;; A collection of data structures for representing note events
;; and their place in time. These can be transformed by the
;; special DSL in note-dsl.scm, or directly by users who don't
;; mind working directly in scheme.
;; ------------------------------------------------------------
(library (note)
  (export priority-keys
          make-note
	  note-get
	  note-set
	  note-update
	  note-check
	  note-optimise
	  note-prioritise
	  
	  note-beat
	  note-before?
	  print-note
	  print-notes
	  make-notes-with-times
	  make-notes-regular

	  window
	  window?
	  make-window
	  window-with-start
	  window-with-end
	  window-start
	  window-end
	  window-valid?
	  within-window?

	  context
	  make-context
	  context-note
	  context-notes-next
	  context-notes-prev
	  context-window
	  context-print
	  context-move
	  context-to-closest-note
	  context-complete?
	  context-rewind)

  (import (chezscheme) (utilities) (srfi s26 cut)
	  (only (srfi s1 lists) delete-duplicates))

  ;; A note event. It's really just an associative dictionary,
  ;; mapping keys to values. The most important key is 'beat'
  ;; which describes its position in time.
  ;; It's implemented as an immutable alist - every operation
  ;; returns a new list, the old one remains untouched.
  (define time-key 'beat)
  (define priority-keys '(length freq beat))

  (define-syntax make-note
    (syntax-rules ()
      ((_ start-beat (key value) ...)
       (list (cons 'beat start-beat) (cons 'key value) ...))
      
      ((_ ...)
       (syntax-error "make-note syntax: (_ 1/16 [:freq 100]"))))

  (define (note-get note key default)
    (let ([result (assq key note)])
      (if result (cdr result) default)))
  (define (note-set note key value)
    (cons (cons key value) note))
  (define (note-update note key update-fn default)
    (note-set note key (update-fn (note-get note key default))))
  (define (note-beat n)
    (note-get n time-key 0))
  (define (note-before? n1 n2)
    (< (note-beat n1) (note-beat n2)))
  ;; Checks there is an item at the key and that it
  ;; satisfies the predicate (which may have extra args)
  (define (note-check note key pred . args)
    (let ([v (note-get note key #f)])
      (if v (apply pred (cons v args)) #f)))

  ;; note-optimise returns an identical-looking note that's
  ;; been cleaned of obsolete mappings and had certain key/value
  ;; pairs moved to the front to accelerate finding common entries.
  (define (note-prioritise note key)
    (let ([result (note-get note key #f)])
      (if (not result) note
	  (note-set note key result))))
  
  (define (note-optimise note)
    (let ([n (fold-left (cut note-prioritise <> <>) note priority-keys)])
      (delete-duplicates n (lambda (x y) (eq? (car x) (car y))))))

  ;; Some note convenience functions
  (define (print-note note)
    (display "[")
    (for-each
     (lambda (p)
       (display " ")
       (display (car p))
       (display ": ")
       (display (cdr p))
       (display ", "))
     (note-optimise note))
    (display "]")
    (newline))

  (define (print-notes note-list)
    (for-each print-note note-list))

  (define (make-notes-with-times times-list)
    (map (lambda (t) (make-note t)) times-list))

  (define (make-notes-regular num interval start)
    (define (impl lst num t)
      (if (= num 0) lst
	  (impl (cons t lst) (sub1 num) (+ t interval))))
    (make-notes-with-times (impl '() num start)))

  ;; A window of time (in beats)
  (define-record-type window
    (fields (immutable start)
	    (immutable end)))

  (define (window-with-start w new-start)
    (make-window new-start (window-end w)))
  (define (window-with-end w new-end)
    (make-window (window-start w) new-end))
  (define (window-valid? w)
    (< (window-start w) (window-end w)))
  (define (within-window? w t)
    (between t (window-start w) (window-end w)))

  ;; Find the window which a list of notes encompasses.
  ;; The call to list-last makes this relatively slow.
  (define (window-from-notes notes)
    (apply make-window
     (if (null? notes)
	 (list 0 0)
	 (list (note-beat (car notes))
	       (note-beat (list-last notes))))))

  ;; ---------------------------------------------
  ;; A context for a note in a series. notes-next contains the
  ;; remaining notes (starting with 'this' note) and prev-notes
  ;; contains the previous notes in reverse, starting with the one
  ;; preceding 'this'.
  (define-record-type context
    (fields (immutable notes-next)
	    (immutable notes-prev)
	    (immutable window)))

  (define (context-note c)
    (car (context-notes-next c)))

  (define (context-print c)
    (let ([win (context-window c)])
      (display "Window: ")
      (display (window-start win))
      (display ", ")
      (display (window-end win))
      (newline)
      (print-notes (context-notes-next c))))

  ;; pass a context and notes-next & notes-prev.
  ;; swap their order to move one item forward or back.
  ;; moves the head of list returned by (get-next c)
  (define (context-move1 c get-next get-prev)
    (let ([next (get-next c)]
	  [prev (get-prev c)])
      (make-context (if (pair? next) (cdr next) '())
		    (if (pair? prev) (cons (car next) prev) '())
		    (context-window c))))

  (define (context-it c direction-fn)
    (let ([n context-notes-next]
	  [p context-notes-prev]
	  [direction (direction-fn c)])
      (if (zero? direction) c
	  (context-it (apply context-move1
			     (if (positive? direction)
				 (list c n p)
				 (list c p n)))
		      direction-fn))))

  ;; direction-fn that drives context iteration forward/back n times,
  ;; depending on whether n is positive or negative, or until the next/prev
  ;; list runs out.
  (define (until-zero-or-end n)
    (lambda (context)
      (let ([x n]
	    [getter (if (positive? n)
			context-notes-next
			context-notes-prev)])
	(cond
	 ((or (zero? x) (null? (getter context))) 0)
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
    (context-it c (until-zero-or-end n)))

  (define (context-to-closest-note c time)
    (context-it c (to-closest time)))

  (define (context-complete? c)
    (eqv? '() (cdr (context-notes-next c))))
  
  (define (context-rewind c)
    (context-move c -9999999)) ;; Lazy

  ) ; end module 'note'
