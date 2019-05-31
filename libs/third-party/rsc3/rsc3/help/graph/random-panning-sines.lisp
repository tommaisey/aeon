(define rps
  (let ((nd (let ((o (f-sin-osc ar (lin-rand 80 2000 0) 0))
                  (l (lf-noise1 kr (rand 0.8 1.2)))
                  (a (lf-noise1 kr (rand 0.82 0.98))))
              (pan2 o l a))))
    (mul (mix (clone 8 nd)) (/ 0.4 8))))

(with-sc3 (overlap-texture-u (list 8 8 2 dinf) rps))
