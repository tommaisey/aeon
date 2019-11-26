;; (.) :: (b -> c) -> (a -> b) -> a -> c
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; const :: a -> b -> a
(define const
  (lambda (x)
    (lambda (_)
      x)))

;; flip :: (a -> b -> c) -> b -> a -> c
(define flip
  (lambda (f)
    (lambda (x y)
      (f y x))))

;; id :: a -> a
(define id
  (lambda (x)
    x))

;; on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(define on
  (lambda (j f)
    (lambda (x y)
      (j (f x) (f y)))))
