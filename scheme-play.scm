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

;; Extend the simple random function that comes with Chez.
(define sys-rand random)

(define random
  (case-lambda
    [() (sys-rand 1.0)]
    [(max) (sys-rand max)]
    [(min max) (+ min (sys-rand (- max min)))]))

;; Some 'english sounding' math operators. These are intended to
;; be intuitive for non-programmers, so aren't always 'precise'.
(define (nearly-divisible a b error)
   (let* ([remain (/ a b)]
	  [truncated (truncate remain)])
     (< (- remain truncated) error)))

(define (divisible a b)
  (nearly-divisible a b 0.0001))

(define (above a b)
  (>= a b))

(define (below a b)
  (<= a b))

(define (nearly-equals a b error)
  (< (abs (- a b)) error))

(define (equals a b)
  (= a b))

(define (within x lower upper)
  (and (>= x lower) (<= x upper)))

;; ------------------------------------------------------------
;; Macros/functions that implement a DSL for specifying/transforming
;; musical patterns. We don't want beginners to ever have to type
;; 'lambda' or scary words like that. We'd prefer that they don't
;; know they're programming at all.
;; ------------------------------------------------------------

;; Defines a function, and also generates an overload that returns
;; a partially-applied unary lambda. Call the overload by omitting
;; the first argument - it becomes the argument to the lambda. Example:
;;
;; (define-unary (my-add a b c) (+ a b c)) 
;; (my-add 4 3 2) ; 9
;; (map (my-add 3 2) '(4 0 1)) ; (9 5 6)
;;
;; TODO: get rid in favour of SRFI 26's cut?
(define-syntax define-unary
  (syntax-rules ()
    
    ((_ (name arg0 args ...) body ...)
     (define name
       (case-lambda
	 [(arg0 args ...) body ...]
	 [(args ...) (lambda (a) (name a args ...))]))) ; partially-applied
    
    ((_ ...)
     (syntax-error
      "define-unary should look like a function definition with 1+ arguments."))))

;; Mini-DSL which lets users modify note hashmaps in this style:
;;
;; (to note
;;  [amp 0.6]
;;  [pan (random -0.3 0.3)]
;;  [freq (* input 2) 440]) ; 440 is the default for `input`
;;
;; Returns a lambda which, when passed a note, updates it with the
;; supplied key-values. Within a value form, the user can use the
;; special keyword `input` to get the current value. They can also
;; supply a default in case there is no existing value.
(define-syntax to
  (lambda (x)
    (syntax-case x ()
      
      ((_ (key value default) rest ...)
       (with-syntax ([input (datum->syntax (syntax key) 'input)])
	 (syntax (lambda (note)
		   (let ([input (note-get note 'key default)])
		     (note-set! note 'key value))
		   ((to rest ...) note)))))

      ((default-0 (key value) rest ...)
       (syntax (to (key value 0) rest ...)))
      
      ((base-case)
       (syntax (lambda (note) note)))

      ((direct-call note rest ...)
       (syntax ((to rest ...) note)))

      ((_ ...)
       (syntax-error
	"'to' should contain a series of key/value pairs.")))))

; with is an alias for 'to'. TODO: should return a new note?
(define-syntax with
  (syntax-rules () ((_ rest ...) (to rest ...))))

;; Logical note operators. Take a note and N [key pred arg] lists.
;; Checks if all/any values at the keys match the predicates.
;; Optionally partially applied.
(define-syntax has
  (syntax-rules ()
    
    ((_ [key pred args ...] rest ...)
     (lambda (n) (and
		     (note-has n 'key)
		     (pred (note-get n 'key #f) args ...)
		     ((has rest ...) n))))

    ((base-case) (lambda (x) #t))
    ((direct-call note pairs ...) ((has pairs ...) note))))

(define-syntax any
  (syntax-rules ()
    
    ((_ [key pred args ...] rest ...)
     (lambda (n) (or
		  (and (note-has n 'key)
		       (pred (note-get n 'key #f) args ...))
		  ((any rest ...) n))))

    ((base-case) (lambda (x) #f))
    ((direct-call note pairs ...) ((any pairs ...) note))))

;;-------------------------------------------------------------
;; Rudimentary note based on hashtable
;; ------------------------------------------------------------
(define note-table-start-size 16)

(define (make-note start-beat)
  (let ([table (make-eq-hashtable note-table-start-size)])
    (hashtable-set! table 'beat start-beat)
    table))

(define (note-has note key)
  (hashtable-contains? note key))
(define (note-get note key default)
  (hashtable-ref note key default))
(define (note-set! note key value)
  (hashtable-set! note key value))
(define (note-update! note key change-fn default)
  (hashtable-update! note key change-fn default))

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
  (map make-note times-list))

(define (make-regular-notes num interval start)
  (define (impl lst num t)
    (if (= num 0) lst
	(impl (cons (make-note t) lst) (sub1 num) (+ t interval))))
  (impl '() num start))

;;-----------------------------------------------------------------
;; Context
;;-----------------------------------------------------------------
;; A window of time (in beats)
(define-record window ((immutable start) (immutable end)))

(define (valid-window? w) (< (window-start w) (window-end w)))
(define (with-window-start w new-start) (make-window new-start (window-end w)))

(define (in-window? w t) (within t ))

;; A context to be passed to a notes pipeline function - the
;; pipeline must add to/transform the notes list.
(define-record context (notes window))

;; TODO: These mutate the input context. Functional instead?
(define-syntax rec-set
  (syntax-rules ()

    ((_ (name record get-fn set-fn) body ...)
     (begin
       (set-fn record ((lambda (name) body ...) (get-fn record)))
       record))))

(define-unary (change-if context match-fn update-fn!)
  (rec-set (notes context context-notes set-context-notes!)
	   (for-each
	    (lambda (n)
	      (when (match-fn n)
		(update-fn! n)))
	    notes)
	   notes))

(define-unary (copy-if context match-fn mutate-fn)
  (rec-set (notes context context-notes set-context-notes!)
	   (define (impl in out)
	     (if (null? in) out
		 (if (match-fn next)
		     (impl (cdr in) (cons (mutate-fn (hashtable-copy (car in) #t)) out))
		     (impl (cdr in) out))))
	   (append (impl notes '()) notes)))

(define-unary (change-all context mutate-fn)
  (change-if context (lambda (x) #t) mutate-fn))

(define-unary (copy-all context mutate-fn)
  (copy-if context (lambda (x) #t) mutate-fn))

(define (print-context c)
  (display "start: ")
  (display (window-start (context-window c)))
  (display ", end: ")
  (display (window-end (context-window c)))
  (newline)
  (print-notes (context-notes c)))

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

(define-unary (is-on note divisor)
  (= 0 (note-time-mod note divisor)))

(define-unary (is-near note divisor nearness)
  (<= (note-time-mod note divisor) nearness))

;;-----------------------------------------------------------------
;; Euclidean patterns
;;-----------------------------------------------------------------
;; Parameters for a euclidean rhythm
(define-record euclid (num-steps num-hits offset))

;; This version would always result in a hit on the last step,
;; but the usual expectation is that it's on the first step
;; TODO: is this necessary? Wouldn't just (add1 offset) work?
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

;; Make the notes inside a window w given a euclidean pattern e
(define (euclidean-rhythm w e stretch)
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
			  (cons (make-note beat) notes)
			  notes)])
	    (next-step notes e (with-window-start w next)))
	  notes))
    (next-step '() e (with-window-start w start-beat))))
