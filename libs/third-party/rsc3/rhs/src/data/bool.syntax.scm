;; (&&) :: Bool -> Bool -> Bool
(define-syntax and2
  (syntax-rules ()
    ((_ p q) (if p q #f))))

(define-syntax and3
  (syntax-rules ()
    ((_ p q r) (and2 p (and2 q r)))))

;; (||) :: Bool -> Bool -> Bool
(define-syntax or2
  (syntax-rules ()
    ((_ p q) (if p p q))))

(define-syntax or3
  (syntax-rules ()
    ((_ p q r) (or2 p (or2 q r)))))
