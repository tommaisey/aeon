;; Default initial parameters.

(hear
 (let ((x (mouse-x kr 20 sample-rate 0 0.1)))
   (mul (lin-cong-c ar x 1.1 0.13 1 0) 0.2)))

;; randomly modulate parameters.

(hear
 (mul (lin-cong-c
       ar
       (mul-add (lf-noise2 kr 1.0) 1e4 1e4)
       (mul-add (lf-noise2 kr 0.1) 0.5 1.4)
       (mul-add (lf-noise2 kr 0.1) 0.1 0.1)
       (lf-noise2 kr 0.1)
       0)
      0.2))

;; As frequency control...

(hear
 (mul (sin-osc ar (mul-add (lin-cong-c
                            ar
                            40
                            (mul-add (lf-noise2 kr 0.1) 0.1 1.0)
                            (mul-add (lf-noise2 kr 0.1) 0.1 0.1)
                            (lf-noise2 kr 0.1)
                            0)
                           500 600) 0)
      0.1))
