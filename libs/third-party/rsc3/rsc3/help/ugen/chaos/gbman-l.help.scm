;; sclang default initial parameters.

(hear (mul (gbman-l ar (mouse-x kr 20 sample-rate 0 0.1) 1.2 2.1) 0.1))

;; Different initial parameters.

(hear (mul (gbman-l ar (mouse-x kr 20 sample-rate 0 0.1) -0.7 -2.7) 0.1))

;; Wait for it...

(hear (mul (gbman-l ar (mouse-x kr 20 sample-rate 0 0.1) 1.2 2.0002) 0.1))

;; As a frequency control

(hear (mul (sin-osc ar (mul-add (gbman-l ar 40 1.2 2.1) 400 500) 0) 0.1))
