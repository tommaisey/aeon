;; vary frequency

(hear
 (let ((x (mouse-x kr 20 sample-rate 0 0.1)))
   (mul (lorenz-l ar x 10 28 2.667 0.05 0.1 0 0) 0.3)))

;; randomly modulate params

(hear
 (mul (lorenz-l
       ar
       sample-rate
       (mul-add (lf-noise0 kr 1) 2 10)
       (mul-add (lf-noise0 kr 1) 20 38)
       (mul-add (lf-noise0 kr 1) 1.5 2)
       0.05
       0.1 0.0 0.0)
      0.1))

;; as a frequency control

(hear
 (let ((x (mouse-x kr 1 200 0 0.1))
       (c (lorenz-l ar x 10 28 2.667 0.05 0.1 0 0)))
   (mul (sin-osc ar (mul-add (lag c 0.003) 800 900) 0) 0.1)))
