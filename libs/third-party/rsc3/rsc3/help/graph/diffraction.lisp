;; diffraction (rd)

(define mk-p
  (lambda ()
    (let* ((x (mouse-x kr 0.001 0.02 1 0.1))
           (y (mouse-y kr 120 400 1 0.1))
           (f (mul (lf-noise0 kr 4) (mce2 32 64)))
           (w (mul (lf-noise0 kr 32) x)))
      (mul (resonz (pulse ar f w)
                   (add y (mul (lf-noise0 kr 2) 0.1))
                   (mul-add (lf-noise0 kr 6) 0.4 0.8))
           0.5))))

(define diffraction
  (let* ((q (comb-n (mk-p)
                    0.2
                    (mul-add (lf-noise0 kr 128) 0.1 0.1)
                    3))
         (r (let* ((x (mouse-x kr 0.75 1.25 1 0.1))
                   (y (mouse-y kr 0.25 1 1 0.1))
                   (f (lambda (_)
                        (mul (sin-osc ar (mul x (rand 50 59)) 0)
                             (mul y (rand 0.04 0.16))))))
              (mce2 (mix-fill 16 f)
                    (mix-fill 12 f)))))
    (add3 (mk-p) q r)))

(hear diffraction)
