;; -*- geiser-scheme-implementation: chez-*-
;;----------------------------------------------------------
;; Fundamental utilities
;; ---------------------------------------------------------
(library (utilities)
  (export fmod % random
	  nearly-divisible divisible on-each
	  above below within
	  equals nearly-equals

	  window
	  window?
	  make-window
	  with-window-start
	  window-start
	  window-end
	  valid-window?
	  in-window?

	  check-type
	  rec-set
	  define-unary)

  (import (chezscheme))

  ;; A negative-aware modulo that works with floats or fracs
  ;; Warning! breaks sometimes due to floating point error.
  ;; e.g. (fmod 4.666666666666666 (/ 4.0 12))
  ;; Need to find a more robust version... Or stick to fracs!
  (define (fmod n mod)
    (- n (* (floor (/ n mod)) mod)))

  ;; We need to use modulo a lot - use the trad symbol
  (define % modulo)

  ;; Extend the simple random function that comes with Chez.
  (define sys-rand random)

  (define rand
    (case-lambda
      [() (sys-rand 1.0)]
      [(max) (sys-rand max)]
      [(min max) (+ min (sys-rand (- max min)))]))

  ;; Some 'english sounding' math operators. These are intended to
  ;; be intuitive for non-programmers, so don't always conform
  ;; to the expectations a programmer might have.
  (define (nearly-divisible val div error)
    (let* ([remain (/ val div)]
	   [truncated (truncate remain)])
      (< (- remain truncated) error)))

  (define (divisible val div)
    (nearly-divisible val div 0.0001))

  (define (on-each val on each)
    (let ([m (fmod val each)])
      (= m on)))

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

  ;; A window of time (in beats)
  (define-record-type window
    (fields (immutable start)
	    (immutable end)))

  (define (with-window-start w new-start)
    (make-window new-start (window-end w)))
  (define (valid-window? w)
    (< (window-start w) (window-end w)))
  (define (in-window? w t)
    (within t (window-start w) (window-end w)))

  ;; Throw an error if the wrong type is used
  (define (check-type pred val string)
    (unless (pred val) (raise string)))

  ;; Mutate a record by getting, doing your work, and setting.
  ;; You must supply a name to bind the value to, the record,
  ;; and a getter and setter method. Your body must return the
  ;; new value.
  (define-syntax rec-set
    (syntax-rules ()

      ((_ (name record get-fn set-fn) body ...)
       (begin
	 (set-fn record ((lambda (name) body ...) (get-fn record)))
	 record))

      ((_ ...)
       (syntax-error
	"rec-set should have a list of 4 items before the body"))))

  ;; Defines a function, and also generates an overload with one
  ;; fewer arguments. The overload returns a unary lambda, which
  ;; can have the missing (first) argument applied to get the result.
  ;;
  ;; (define-unary (my-add a b c) (+ a b c)) 
  ;; (my-add 4 3 2)   ; 9
  ;; ((my-add 3 2) 4) ; 9
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
  
  ) ; end module 'utilities'
