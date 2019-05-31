;; what was i thinking? (jmcc) #2

(define what-was-i-thinking
  (let* ((n0 (mul-add (lf-noise1 kr 0.2) 2000 2400))
         (n1 (mul-add (lf-noise1 kr 0.157) 0.4 0.5))
         (i (mul3 (lf-pulse ar 0.1 0 0.05) (impulse ar 8 0) 500))
         (d (decay i 2))
         (f (u:max (add (sin-osc kr 4 0) 80) d))
         (p (mul (pulse ar f n1) 0.04))
         (z (rlpf p n0 0.2))
         (c (lambda (x)
              (let* ((r (rand 0 0.3))
                     (n (mul-add (lf-noise1 kr r) 0.025 0.035)))
                (comb-l x 0.06 n 1))))
         (y (mul z 0.6)))
    (add z (mce2 (add (c y) (c y))
                 (add (c y) (c y))))))

(hear what-was-i-thinking)
