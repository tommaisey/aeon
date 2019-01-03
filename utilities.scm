;; -*- geiser-scheme-implementation: chez-*-
;;----------------------------------------------------------
;; Fundamental utilities
;; ---------------------------------------------------------
(library (utilities)
  (export fmod % rand
	  nearly-divisible divisible on-each
	  above below between
	  equals nearly-equals
	  column-process
	  merge-columns
	  columns-to-rows
	  merge-inner
	  sorted?
	  list-last
	  remove-list
	  check-type)

  (import (chezscheme) (srfi s26 cut))

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

  (define (between x lower upper)
    (and (>= x lower) (<= x upper)))

  ;; Takes the head off each inner list until one of
  ;; them runs out. The heads of each column are offered
  ;; to a joiner func, along with an accumulated result.
  (define (column-process col-list joiner)
    (let impl ([l col-list] [result '()])
      (if (member '() l) result
	  (impl (map cdr l) (joiner result (map car l))))))
  
  ;; ((1 2 3) (x y) (a b c)) -> (1 x a 2 y b)
  (define (merge-columns col-list)
    (column-process col-list (lambda (a b) (append a b))))

  ;; ((1 2 3) (x y) (a b c)) -> ((1 x a) (2 y b))
  (define (columns-to-rows col-list)
    (reverse (column-process col-list (lambda (a b) (cons b a)))))

  ;;  ((1 2 3) (x y) (a b c)) -> (1 2 3 x y a b c)
  (define (merge-inner col-list)
    (fold-left append '() col-list))

  ;; Check if a list is sorted or not.
  (define (sorted? less? l)
    (let impl ([l (cdr l)] [prev (car l)])
      (cond
       ((null? l) #t)
       ((or (less? prev (car l))
	    (not (less? (car l) prev)))
	(impl (cdr l) (car l)))
       (else #f))))

  ;; Get the last element of a list. Slow operation.
  (define (list-last l)
    (if (null? (cdr l))
	(car l)
        (list-last (cdr l))))

  ;; Remove matching items in b from a
  (define (remove-list a b)
    (filter (lambda (x) (not (member x b))) a))

   ;; Throw an error if the wrong type is used
  (define (check-type pred val string)
    (unless (pred val) (raise string)))
  
  ) ; end module 'utilities'
