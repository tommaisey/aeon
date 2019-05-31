;; klink (rd)

(define klink
  (let* ((n1 (lf-noise0 kr (mce2 0.5 1.5)))
         (o (sin-osc kr n1 0))
         (f (mce2 2 3))
         (a (mul (u:abs (slope o)) f))
         (t (impulse ar a 0))
         (i (decay2 t 0.01 0.1))
         (x (mouse-x kr 960 3620 1 0.1))
         (y (mouse-y kr 0.5 2.0 0 0.2))
         (n2 (t-rand x 3940 t))
         (n3 (t-rand 0.005 0.275 t)))
    (ringz i n2 (mul n3 y))))

(hear klink)
