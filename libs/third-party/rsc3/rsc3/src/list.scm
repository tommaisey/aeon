;; [a] -> int -> [a]
(define extend
  (lambda (l n)
    (let ((z (length l)))
      (cond ((= z n) l)
	    ((> z n) (take n l))
	    (else (extend (append l l) n))))))

;; [a] -> int -> [a]
(define take-cycle
  (lambda (l n)
    (if (null? l)
	nil
	(cons (head l)
	      (take-cycle (drop n l) n)))))

;; (a -> a -> a) -> ([a] -> [a])
(define differentiate-with
  (lambda (f)
    (lambda (l)
      (zip-with f l (cons 0 l)))))

;; num a => [a] -> [a]
;;
;; (equal? (differentiate '(1 2 4 7 11)) '(1 1 2 3 4))
(define differentiate
  (differentiate-with -))

;; (a -> a -> a) -> ([a] -> [a])
(define integrate-with
  (lambda (f)
    (lambda (l)
      (let ((x (car l))
            (xs (cdr l))
            (g (lambda (a x) (let ((y (f a x))) (cons y y)))))
        (cons x (cdr (map-accum-l g x xs)))))))

;; num a => [a] -> [a]
;;
;; (equal? (integrate (list 3 4 1 1)) (list 3 7 8 9))
;; (equal? (integrate '(1 1 2 3 4)) '(1 2 4 7 11))
(define integrate
  (integrate-with +))

(define d->dx
  (lambda (l)
    (zip-with sub (drop 1 l) l)))

;; int -> [any] -> [any]
(define without
  (lambda (n l)
    (append (take n l) (drop (+ n 1) l))))

;; [int] -> bool
(define consecutive?
  (lambda (l)
    (let ((x (head l))
	  (xs (tail l)))
      (or (null? xs)
	  (and (= (+ x 1) (head xs))
	       (consecutive? xs))))))
