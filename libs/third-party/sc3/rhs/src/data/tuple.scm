;; (define-record-type duple
;;   (fields p q))

;; (srfi:define-record-type duple
;;   (make-duple p q) duple? (p duple-p) (q duple-q))

;; fst :: (a, b) -> a
;; (define fst car)

;; snd :: (a, b) -> b
;; (define snd cdr)

;; (,) :: a -> b -> (a, b)
;; (define tuple2 cons)

;; curry :: ((a, b) -> c) -> a -> b -> c
(define curry
  (lambda (f)
    (lambda (x y)
      (f (cons x y)))))

;; uncurry :: (a -> b -> c) -> (a, b) -> c
(define uncurry
  (lambda (f)
    (lambda (c)
      (f (car c) (cdr c)))))
