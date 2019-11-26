;; With default initial parameters.

(hear (mul (henon-n ar (mouse-x kr 20 sample-rate 0 0.1) 1.4 0.3 0 0) 0.1))

;; With mouse-control of parameters.

(hear
 (mul (henon-n ar
               (fdiv sample-rate 4)
               (mouse-x kr 1 1.4 0 0.1)
               (mouse-y kr 0 0.3 0 0.1)
               0
               0)
      0.1))

;; With randomly modulate parameters.

(hear
 (mul (henon-n ar
               (fdiv sample-rate 8)
               (mul-add (lf-noise2 kr 1) 0.20 1.20)
               (mul-add (lf-noise2 kr 1) 0.15 0.15)
               0
                    0)
      0.1))

;; As a frequency control.

(let ((x (mouse-x kr 1 1.4 0 0.1))
      (y (mouse-y kr 0 0.3 0 0.1))
      (f 40))
  (hear (mul (sin-osc ar (mul-add (henon-n ar f x y 0 0) 800 900) 0) 0.1)))
