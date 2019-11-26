;; int -> (() -> a) -> [a]
(define replicate-m*
  (lambda (i x)
    (if (<= i 0)
        nil
        (cons (x) (replicate-m* (- i 1) x)))))
