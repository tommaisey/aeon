;; data Ordering = LT | EQ | GT

;; compare :: (Ord a) => a -> a -> Ordering
(define compare
  (lambda (x y)
    (cond ((> x y) 'gt)
          ((< x y) 'lt)
          (else 'eq))))

;; max :: a -> a -> a
(define max (lambda (x y) (if (> x y) x y)))

;; min :: a -> a -> a
(define min (lambda (x y) (if (< x y) x y)))
