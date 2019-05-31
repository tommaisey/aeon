(define cusp_ cusp-l)

;; vary frequency

(hear
 (let ((x (mouse-x kr 20 sample-rate 0 0.1)))
   (mul (cusp_ ar x 1.0 1.99 0) 0.1)))

;; mouse-controlled params

(hear
 (let ((x (mouse-x kr 0.9 1.1 1 0.1))
       (y (mouse-y kr 1.8 2 1 0.1)))
   (mul (cusp_ ar (fdiv sample-rate 4) x y 0) 0.1)))

;; as a frequency control

(hear
 (let* ((x (mouse-x kr 0.9 1.1 1 0.1))
        (y (mouse-y kr 1.8 2 1 0.1))
        (f (mul-add (cusp_ ar 40 x y 0) 800 900)))
   (mul (sin-osc ar f 0.0) 0.1)))
