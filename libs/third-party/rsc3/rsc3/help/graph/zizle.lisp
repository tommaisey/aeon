;; zizle (jmcc) #SC3d1.5

(define rrand rand-range)

(define zizle
  (let* ((a (lambda (f)
              (let ((freq (mul f (mce2 (rrand 0.7 1.3) 1)))
                    (ph (mce2 (rand* two-pi) (rand* two-pi))))
                (mix (mul (sin-osc ar freq ph) 0.1)))))
         (a1 (u:max (a (exp-rand 0.38 8)) 0))
         (a2 (u:abs (a (exp-rand 6 24)))))
    (pan2 (sin-osc ar (midi-cps (rrand 24 108)) (rand* two-pi)) (rand2 1) (mul a1 a2))))

(with-sc3 (overlap-texture-u (list 4 4 12 +inf.0) zizle))
