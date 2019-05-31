;; (trunc a b)

;; Truncate a to a multiple of b.

(let* ((x (mouse-x kr 60 4000 0 0.1))
       (f (trunc x 100)))
  (audition
   (out 0 (mul (sin-osc ar f 0) 0.1))))
