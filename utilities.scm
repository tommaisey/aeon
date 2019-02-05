;; -*- geiser-scheme-implementation: chez-*-
;;----------------------------------------------------------
;; Fundamental utilities
;; ---------------------------------------------------------
(library (utilities)
  (export fmod % pseudo-rand
	  nearly-divisible divisible on-each
	  above below between
	  equal nearly-equal
	  column-process
	  merge-columns
	  columns-to-rows
	  merge-inner
	  merge-sorted
	  sorted?
	  for-any
	  for-none
	  combine-preds
	  list-last
	  remove-list
	  push-front
	  check-type)

  (import (chezscheme) (srfi s27 random-bits))

  ;; A negative-aware modulo that works with floats or fracs
  ;; Warning! breaks sometimes due to floating point error.
  ;; e.g. (fmod 4.666666666666666 (/ 4.0 12))
  ;; Need to find a more robust version... Or stick to fracs!
  (define (fmod n mod)
    (- n (* (floor (/ n mod)) mod)))

  ;; We need to use modulo a lot - use the trad symbol
  (define % modulo)

  ;; A pseudo-random number generator that takes a seed.
  (define pseudo-rand-src (make-random-source))
  
  (define (pseudo-rand min max seed)
    (let* ([i (exact (truncate seed))]
	   [j (exact (truncate (* 100 (- seed i))))]
	   [len (- max min)])
      (random-source-pseudo-randomize! pseudo-rand-src i j)
      (+ min (if (and (exact? min) (exact? max))
		 ((random-source-make-integers pseudo-rand-src) len)
		 (* len ((random-source-make-reals pseudo-rand-src)))))))

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

  (define (nearly-equal a b error)
    (< (abs (- a b)) error))

  (define (equal a b)
    (= a b))

  (define (not-equal a b)
    (not (equal)))

  (define (between x lower upper)
    (and (>= x lower) (<= x upper)))

  (define (between-each val each lower upper)
    (let ([m (fmod val each)])
      (between m lower upper)))

  ;; It's going to be more readable for users to write 'pair' than 'cons'.
  (define pair cons)

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
    (fold-right (lambda (x v)
		 (if (proper? x)
		     (append x v)
		     (append (list x) v)))
	       '() col-list))

  ;; Check if something is a list but not a pair (or any other type). 
  (define (proper? pair)
    (and (pair? pair)
	 (or
	  (eq?   (cdr pair) '())
	  (pair? (cdr pair)))))

  ;; Check if a list is sorted or not.
  (define (sorted? less? l)
    (let recur ([l (cdr l)] [prev (car l)])
      (cond
       ((null? l) #t)
       ((or (less? prev (car l))
	    (not (less? (car l) prev)))
	(recur (cdr l) (car l)))
       (else #f))))

  ;; Stable merges to lists according to less?. Lifted from
  ;; SRFI95 ref implementation, with tweaks.
  (define (merge-sorted a b less? . opt-key)
    (define key (if (null? opt-key) values (car opt-key)))
    (cond ((null? a) b)
	  ((null? b) a)
	  (else
	   (let loop ((x (car a)) (kx (key (car a))) (a (cdr a))
		      (y (car b)) (ky (key (car b))) (b (cdr b)))
	     ;; The loop handles the merging of non-empty lists.  It has
	     ;; been written this way to save testing and car/cdring.
	     (if (less? ky kx)
		 (if (null? b)
		     (cons y (cons x a))
		     (cons y (loop x kx a (car b) (key (car b)) (cdr b))))
		 ;; x <= y
		 (if (null? a)
		     (cons x (cons y b))
		     (cons x (loop (car a) (key (car a)) (cdr a) y ky b))))))))

  ;; R6RS provides for-all, which checks all items in a
  ;; list return true for pred. Here's the 'none' and
  ;; 'any' versions.
  (define (for-any pred lst)
    (exists pred lst))

  (define (for-none pred lst)
    (not (for-any pred lst)))

  ;; Get a lambda that combines all of the predicates in the
  ;; list. e.g. Supplying for-all/any will check
  ;; all/any of the predicates return true for x.
  (define (combine-preds preds for-all/any/none)
    (lambda (x)
      (for-all/any/none (lambda (p) (p x)) preds)))

  ;; Get the last element of a list. Slow operation.
  (define (list-last l)
    (if (null? (cdr l))
	(car l)
        (list-last (cdr l))))

  ;; Remove matching items in b from a
  (define (remove-list a b)
    (filter (lambda (x) (not (member x b))) a))

  ;; Adds the element to the list. If the element is a list, it is appended.
  (define (push-front val list)
    ((if (list? val) append cons) val list))

   ;; Throw an error if the wrong type is used
  (define (check-type pred val string)
    (unless (pred val) (raise string)))
  
  ) ; end module 'utilities'
