;; A library of 'contextual' functions which return a value based
;; on the context they are passed. This lets us maintain the referential
;; transparency that is key for the system to work.
;;
;; Many of these functions base their value on the time/beat of the current
;; note in the context, unless passed one or more extra keys to look at.
;;
;; Others of these functions don't really need a context - but may want to treat
;; their arguments as c-vals. e.g. round
;;
;; For functions taking some kind of list, we define macros that use auto-quasi -
;; this frees the user from needing to (quasi)quote those input lists. 
(library (c-vals)
  (export rnd pick each snap)
  (import (scheme) (utilities) (context) (note)
	  (for (auto-quasi) expand))

  ;; A random number
  (define rnd
    (case-lambda
      [() (rnd '())]
      [(key) (rnd 0.0 1.0 key)]
      [(min max) (rnd min max '())]
      [(min max key/keys)
       (lambda (context)
	 (let ([seed (fold-by-keys * 10000 key/keys context)])
	   (pseudo-rand min max seed)))]))

  ;; Choose from a list randomly
  (define-syntax pick
    (syntax-rules ()
      ((_ qlist) (pick qlist '()))

      ((_ qlist key/keys)
       (let* ([lst (pdef-quasi qlist)]
	      [len (length lst)])
	 (lambda (context)
	   (list-nth lst ((rnd 0 len key/keys) context)))))))

  ;; Choose from a list according to the current measure
  (define-syntax each
    (syntax-rules ()
      ((_ measures qlist)
       (let* ([lst (pdef-quasi qlist)]
	      [len (length lst)])
	 (lambda (context)
	   (let* ([t (note-beat (context-note context))]
		  [n (exact (truncate (/ t measures)))])
	     (get-c-val (list-nth lst (modulo n len)) context)))))))

  ;; Snap the input value to the next number divisible by divisor.
  (define (snap divisor c-val)
    (lambda (context)
      (let* ([val (get-c-val c-val context)]
	     [overlap (fmod val divisor)]
	     [prev (- val overlap)])
	(if (>= overlap (* 0.5 divisor))
	    (+ prev divisor) prev))))

  ;; Some c-vals allow the user to specify which properties of the
  ;; context's current note are considered when contextualising. This
  ;; makes the implementation of that simpler.
  (define (fold-by-keys fn init key/keys context)
    (define (matches-key? pair)
      (find (lambda (k) (eq? k (car pair))) key/keys))
    (let ([note (context-note context)])
      (cond
       ((null? key/keys)
	(fn init (note-beat note)))
       ((symbol? key/keys)
	(fn init (note-get note key/keys 1)))
       ((unsafe-list? key/keys)
	(fold-left fn init (note-clean (filter matches-key? note)))))))

  
  )
