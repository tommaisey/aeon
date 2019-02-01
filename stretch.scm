
(library (stretch)
  (export stretch)
  (import (scheme) (utilities) (context))

  (define rest-symbol '~)

  ;; Allows definition of nested lists without (quasi)quoting.
  ;; Lists that start with an identifier are evaluated.
  ;; Lists that don't start with an identifier are just a list.
  ;;
  ;; (auto-quasi (1 2 "hi" (+ 2 2) (5 (+ 5 5)))) => (1 2 "hi" 4 (5 10))
  (define-syntax auto-quasi
    (lambda (x)
      (syntax-case x ()
	((_ (v rest ...))
	 (identifier? (syntax v))
	 (syntax (v rest ...))) ; identifier first, call as lambda
	
	((_ (v ...))
	 (syntax (list (auto-quasi v) ...))) ;; make list
     
        ((_ v)
	 (syntax v))))) ;; normal value in a list

  ;; Returns a list of x repeated n times.
  ;;
  ;; (subdiv 3 5) => (5 5 5)
  (define (subdiv n x)
    (let loop ([n n] [o '()])
      (if (= 0 n) o (loop (- n 1) (cons x o)))))
  
  ;; Turn a pattern-def into a list of time segments paired with the original
  ;; value of each item in the pattern. If do-subdiv is #t, numeric items
  ;; greater than 1 are considered instructions to subdivide the step by N.
  ;;
  ;; (psegment '[~ 3] 1 #f) => ((1/2 . ~) (1/2 . 3))
  ;; (psegment '[~ 3] 1 #t) => ((1/2 . ~) (1/6 . 3) (1/6 . 3) (1/6 . 3))
  (define (psegment p t do-subdiv)
    (cond
     ((pair? p)
      (let ([subseg (lambda (sub-p) (psegment sub-p (/ t (length p)) do-subdiv))])
	(merge-inner (map subseg p))))
     ((and do-subdiv (number? p) (> p 1))
      (subdiv p (cons (* t (/ 1 p)) p)))
     (else (cons t p))))

  ;; Builds up a list according to the input segments, the active time range,
  ;; and a convert-fn to process/filter each segment's time & value.
  ;; The convert-fn should return #f for items that shouldn't be added, or the
  ;; value to be added otherwise (e.g. a note).
  ;; See @pfill-notes and @pfill-edits for examples.
  (define (pfill segs len start end convert-fn)
    (define get-seg-t car)
    (define get-seg-v cdr)
    (if (and (> end start)
	     (pair? segs))
	(let loop ([s segs]
		   [t (exact (truncate (/ start len)))]
		   [o '()])
	  (cond
	   ((null? s) (loop segs t o))
	   ((>= t end) (reverse o))
	   (else (let* ([seg-len (get-seg-t (car s))]
			[t-n (+ t seg-len)]
			[v-n (convert-fn t seg-len (get-seg-v (car s)))]
			[o-n (if v-n (cons v-n o) o)])
		   (loop (cdr s) t-n o-n)))))
	'()))

  ;; Build a list of notes with appropriate times from some segments & stretch info.
  (define (pfill-notes segs len start end)
    (define (make-rest-or-note t seg-len val)
      (if (or (eq? val rest-symbol)
	      (not (between t start end)))
	  #f (make-note t)))
    (pfill segs len start end make-rest-or-note))

  ;; Builds a function that transforms a context from some segments whose values
  ;; should be functions taking a context and returning a new note with the required
  ;; changes applied. The output function applies the correct note-transform fn for
  ;; each note in the context based on each note's time.
  ;;
  ;; TODO...
  (define (pfill-edits segs len start end)
    (list))
  
  (define (build-event-pattern pattern-def stretch-def-args)
    (lambda (context)
      (let* ([times (range-start (context-range context))]))))

  )
