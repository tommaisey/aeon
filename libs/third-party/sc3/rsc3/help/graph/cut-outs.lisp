;; cut-outs (rd)

(define cut-outs
  (let* ((t (mul (impulse ar 22 0) (add (sin-osc kr 0.5 0) 1)))
         (x (mouse-x kr 0.005 0.12 1 0.1))
         (y (mouse-y kr 0.01 0.52 1 0.1))
         (n (lambda (_)
              (let* ((n1 (lf-noise0 kr 2))
                     (n2 (coin-gate (add3 0.05 (mul n1 0.4) y) (mul t 0.5)))
                     (n3 (t-exp-rand (mce2 500 900) 1600 t)))
                (ringz n2 n3 x))))
         (s (mix-fill 3 n))
         (b (t-rand 0 1 (dust kr 8))))
    (make-mrg (mul (clip2 s (in 1 kr 0)) 0.25)
              (out 0 b))))

(hear cut-outs)
