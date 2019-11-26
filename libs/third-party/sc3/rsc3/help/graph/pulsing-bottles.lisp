;; pulsing bottles (jmcc) #2

(define u
  (lambda (_)
    (let* ((n (white-noise ar))
           (r0 (rand 4 14))
           (r1 (rand 0 0.7))
           (r2 (rand 400 7400))
           (r (resonz (mul3 n (lf-pulse kr r0 0 0.25) r1) r2 0.01))
           (f (rand 0.1 0.5))
           (p (rand 0 (mul pi 2)))
           (s (sin-osc kr f p)))
      (pan2 r s 1))))

(define pulsing-bottles (mix-fill 6 u))

(hear pulsing-bottles)
