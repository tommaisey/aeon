;;----------------------------------------------------------------------
;; leaves
;;
;; These are 'contextual' values - functions which return a value based
;; on the context they are passed. This lets us maintain the referential
;; transparency that is key for the system to work.
;;
;; They form the 'leaves' of a tree of functions defining musical patterns.
;;
;; Many of these functions base their value on the time/beat of the current
;; event in the context, unless passed one or more extra keys to look at.
;;
;; Others of these functions don't really need a context - but may want to
;; treat their arguments as callable leaves.
;;----------------------------------------------------------------------
(library (leaf)
  (export
   this next nearest
   c+ c- c* c/
   rnd pick each snap)
  
  (import
    (chezscheme) (utilities) (context) (event)
    (for (auto-quasi) expand))

  (define (get c key default)
    (event-get (context-event c) key default))

  ;; Get values from the current or neighbouring events in the context.
  (define (this key default)
    (lambda (context)
      (get context key default)))

  (define (next idx key default)
    (lambda (context)
      (get (context-move context idx) key default)))

  (define (nearest time key default)
    (lambda (context)
      (get (context-to-closest-event context time) key default)))

  ;;-------------------------------------------------------------------
  ;; Maths
  (define (leaf-apply fn leaves)
    (lambda (context)
      (apply fn (map (lambda (v) (get-leaf v context)) leaves))))

  (define (c+ . leaves)
    (leaf-apply + leaves))
  
  (define (c- . leaves)
    (leaf-apply - leaves))

  (define (c* . leaves)
    (leaf-apply * leaves))

  (define (c/ . leaves)
    (leaf-apply / leaves))

  ;; Snap the input value to the next number divisible by divisor.
  (define (snap divisor val)
    (lambda (context)
      (let* ([val (get-leaf val context)]
	     [divisor (get-leaf divisor context)]
	     [overlap (fmod val divisor)]
	     [prev (- val overlap)])
	(if (>= overlap (* 0.5 divisor))
	    (+ prev divisor) prev))))

  ;;-------------------------------------------------------------------
  ;; Pseudo-random values and choices.
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
	   (get-leaf (list-nth lst ((rnd 0 len key/keys) context)) context))))))

  ;;--------------------------------------------------------------------
  ;; Rhythmic & sequencing operations.
  ;; Choose from a list according to the current measure
  (define-syntax each
    (syntax-rules ()
      ((_ measures qlist)
       (let* ([lst (pdef-quasi qlist)]
	      [len (length lst)])
	 (lambda (context)
	   (let* ([t (context-now context)]
		  [n (exact (truncate (/ t measures)))])
	     (get-leaf (list-nth lst (modulo n len)) context)))))))

  ;; Some leaves allow the user to specify which properties of the
  ;; context's current event are considered when contextualising. This
  ;; makes the implementation of that simpler.
  (define (fold-by-keys fn init key/keys context)
    (define (matches-key? pair)
      (find (lambda (k) (eq? k (car pair))) key/keys))
    (let ([time (context-now context)]
	  [event (context-event context)])
      (cond
       ((null? key/keys)
	(fn init time))
       ((symbol? key/keys)
	(fn init (event-get event key/keys 1)))
       ((unsafe-list? key/keys)
	(fold-left fn init (event-clean (filter matches-key? event)))))))
  
  )
