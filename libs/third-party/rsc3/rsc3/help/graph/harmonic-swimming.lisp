;; harmonic swimming (jmcc) #1

(define harmonic-swimming
  (let* ((a 0.02)
         (f 50)
         (p 20)
         (z 0)
         (l (line kr 0 (neg a) 60 0))
         (o (lambda (h)
              (let* ((r (clone 2 (rand 2 8)))
                     (n (lf-noise1 kr r))
                     (e (u:max 0 (mul-add n a l))))
                (mul (f-sin-osc ar (* f (+ h 1)) 0) e)))))
    (mix-fill p o)))

(hear harmonic-swimming)
