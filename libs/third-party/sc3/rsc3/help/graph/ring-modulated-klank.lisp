;; ring modulated klank (jmcc) #2

(define rmk
  (let* ((p 8)
         (z (klank-data-mce (rand-n p 100 10000)
                            (clone p 1)
                            (rand-n p 0.2 1)))
         (k (klank (mul (dust ar 20) 0.02) 1 0 1 z))
         (f (mul-add (lf-noise2 kr (rand 0.1 0.4)) 200 (rand 350 400))))
    (pan2 (mul (sin-osc ar f 0) k) (rand -1 1) 1)))

(with-sc3 (overlap-texture-u (list 4 4 4 +inf.0) rmk))
