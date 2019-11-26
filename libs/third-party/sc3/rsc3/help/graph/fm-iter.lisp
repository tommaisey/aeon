;; fm-iter (rd)

(define fm-iter
  (let* ((t0 (impulse ar (recip 0.30) 0))
         (t1 (t-delay t0 0.15))
         (t (mce2 t0 t1))
         (k (t-rand 56 57 t))
         (i (t-rand 40 480 t))
         (j (t-rand -1 1 t))
         (c (midi-cps k))
         (m (midi-cps (add3 k 1 j)))
         (s (env-perc 0.01 0.9 1 (list -4 -4)))
         (e (env-gen ar t 0.1 0 1 remove-synth s))
         (f (mul-add (sin-osc ar c 0) i m)))
    (mul (sin-osc ar f 0) e)))

(hear fm-iter)
