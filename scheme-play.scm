;; -*- geiser-scheme-implementation: chez-*-
(optimize-level 2)

;;----------------------------------------------------------
;; Fundamental utilities
;; ---------------------------------------------------------
;; Throw an error if the wrong type is used
(define (check-type pred val string)
  (unless (pred val) (raise string)))

;; A negative-aware modulo that works with floats or fracs
;; Warning! breaks sometimes due to floating point error.
;; e.g. (fmod 4.666666666666666 (/ 4.0 12))
;; Need to find a more robust version...
(define (fmod n mod)
  (- n (* (floor (/ n mod)) mod)))

;; We need to use modulo a lot - use the trad symbol
(define % modulo)

;; Takes a lambda that takes a key and a value, applies to each hashtable entry
(define (hashtable-for-each hashtable fn)
  (call-with-values (lambda () (hashtable-entries hashtable))
    (lambda (keys values) (vector-map fn keys values))))

;; ------------------------------------------------------------
;; Macros that make it easier to share code between magic/beginner
;; mode lens definitions and full/scheme mode. For example, we don't
;; want beginners to ever have to type 'lambda'. So we have macros
;; that transform more 'english' looking expressions into lambdas.
;; ------------------------------------------------------------

;; Defines a function, and also generates an overload that returns
;; a unary lambda. Call the overload by omitting the first argument -
;; this becomes the argument to the lambda. Example:
;;
;; (define-unary (my-add a b c) (+ a b c)) 
;; (my-add 4 3 2) ; 9
;; (map (my-add 3 2) '(4 0 1)) ; (9 5 6)
(define-syntax define-unary
  (syntax-rules ()
    
    ((define-unary (name arg1 . args) body)
     (define name
       (case-lambda
	 [(arg1 . args) body]
	 [args (lambda (x) (name x . args))])))
    
    ((define-unary ...)
     (syntax-error "define-unary should look like a function definition with 1+ arguments."))))

;; Mini-DSL letting users write something like this:
;;
;; (with: [amp 0.6] [freq (* current 2)] [pan (random -0.3 0.3)])
;;
;; Returns a lambda which, when passed a note, updates it with the
;; key-values supplied. TODO: let user reference `current` value for
;; each key. Need to break hygene with syntax-case I think...
(define-syntax with:
  (syntax-rules ()
    
    ((with: (key value) . rest)
     (lambda (n)
       (note-set! n 'key value)
       ((with: . rest) n)))

    ((with:) (lambda (n) n)) ; base case
    
    ((with: ...)
     (syntax-error "note-update: must contain a series of key/value pairs."))))

;;-------------------------------------------------------------
;; Rudimentary note for testing
;; ------------------------------------------------------------
(define note-table-start-size 16)

(define (make-note key value)
  (let ([ht (make-eq-hashtable note-table-start-size)])
    (hashtable-set! ht key value)
    ht))

(define (note-get note key default) (hashtable-ref note key default))
(define (note-set! note key value) (hashtable-set! note key value))
(define (note-update! note key change-fn default) (hashtable-update! key change-fn default))

(define (print-note note)
  (begin
    (display "{")
    (hashtable-for-each note (lambda (key value)
			       (display " ")
			       (display key)
			       (display ": ")
			       (display value)
			       (display ", ")))
    (display "}")
    (newline)))

(define (print-notes note-list)
  (for-each print-note note-list))

(define (make-notes-with-times times-list)
  (map (lambda (t) (make-note 'beat t)) times-list))

(define-unary (notes-set notes pred update-note-fn!)
  (for-each (lambda (n) (when (pred n) (update-note-fn! n))) notes))

;;----------------------------------------------------------
;; Time helper functions
;; ---------------------------------------------------------
;; Snap a value to the next number divisible by divisor.
(define (snap-next beat divisor)
  (let ([overlap (fmod beat divisor)])
    (+ (- beat overlap) divisor)))

;; Snap a value to the next number divisible by divisor,
;; if `beat` isn't already cleanly divisible by divisor.
(define (snap-next-if beat divisor)
  (let ([next (snap-next beat divisor)])
    (if (= beat (- next divisor)) beat next)))

;; Snap a value to the next number divisible by divisor.
(define (snap-prev beat divisor)
  (let ([overlap (fmod beat divisor)])
    (- beat overlap)))

;; Snap a value to the next number divisible by divisor,
;; if `beat` isn't already cleanly divisible by divisor
(define (snap-prev-if beat divisor)
  (let ([prev (snap-prev beat divisor)])
    (if (= beat (+ prev divisor)) beat prev)))

;; Snap to the next or previous divisor, whichever's closer.
(define (snap-nearest beat divisor)
  (let* ([overlap (fmod beat divisor)]
	 [prev (- beat overlap)])
    (if (>= overlap (* 0.5 divisor))
	(+ prev divisor) prev)))

;; Extract the time parameter from a note, fmod it with divisor
(define (note-time-mod note divisor)
  (fmod (note-get note 'beat #f) divisor))

(define-unary (at? note divisor)
  (= 0 (note-time-mod note divisor)))

(define-unary (near? note divisor nearness)
  (<= (note-time-mod note divisor) nearness))

;; represents a window of time in beats
(define-record window ((immutable start) (immutable end)))

(define (valid-window? w) (< (window-start w) (window-end w)))
(define (with-window-start w new-start) (make-window new-start (window-end w)))

;; parameters for a euclidean rhythm
(define-record euclid (num-steps num-hits offset))

;; this version would always result in a hit on the last step,
;; but the usual expectation is that it's on the first step
(define (euclid-normalise-offset e)
  (let ([new-offset (% (add1 (euclid-offset e)) (euclid-num-steps e))])
    (make-euclid (euclid-num-steps e) (euclid-num-hits e) new-offset)))

;; non-iterative derivation of this algorithm:
;; http://www.computermusicdesign.com/simplest-euclidean-rhythm-algorithm-explained/
(define (euclidean-hit? step e)
  (check-type euclid? e "Parameter 'e' must be a euclidean record")
  
  (let*
      ([e      (euclid-normalise-offset e)]
       [hits   (euclid-num-hits e)]
       [steps  (euclid-num-steps e)]
       [offset (euclid-offset e)]
       ;; put bucket in state as if it had done N = offset iterations 
       [bucket (% (* hits (- steps offset)) steps)]
       ;; compute state of bucket just before the requested step
       [bucket (+ bucket (* hits (% step steps)))])
    
    (not (eqv? (quotient bucket steps)
	       (quotient (+ bucket hits) steps)))))

;; Get the notes inside a window w given a euclidean pattern e
(define (fill-euclidean e w stretch)
  (check-type euclid? e "Parameter 'e' must be a euclid record")
  (check-type window? w "Parameter 'w' must be a window record")

  (let*
      ([num-steps  (euclid-num-steps e)]
       [step-size  (/ stretch num-steps)]
       [start-beat (snap-next-if (window-start w) step-size)])
    (define (next-step notes e w)
      (if (valid-window? w)
	  (let*
	      ([beat  (window-start w)]
	       [next  (snap-next beat step-size)]
	       [step  (% (round (/ beat step-size)) num-steps)]
	       [notes (if (euclidean-hit? step e)
			  (cons (make-note 'beat beat) notes)
			  notes)])
	    (next-step notes e (with-window-start w next)))
	  notes))
    (next-step '() e (with-window-start w start-beat))))
