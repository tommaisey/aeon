;; ccomb (rd)

(define ccomb
  (let* ((rng (lambda (u l r) (lin-lin u -1 1 l r)))
         (lwr 48)
         (flwr (midi-cps 48))
         (spart (lambda (t)
                  (let* ((n (rng (lf-noise2 kr 0.1) lwr 72))
                         (e (decay2 t 0.01 (t-rand 0.05 0.75 t)))
                         (x (mul (white-noise ar) e))
                         (m (lf-noise2 kr 0.1))
                         (f (lag (midi-cps n) 0.25)))
                    (comb-c x (recip flwr) (recip f) (rng m 1.0 8.0)))))
         (t (dust ar (mce2 0.9 0.8))))
    (mix-fill 7 (lambda (_) (mul (spart t) 0.1)))))

(hear ccomb)

