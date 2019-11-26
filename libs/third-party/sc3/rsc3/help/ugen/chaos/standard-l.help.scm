;; Vary frequency

(hear (mul (standard-l ar (mouse-x kr 20 sample-rate 0 0.1) 1 0.5 0) 0.1))

;; Mouse-controlled parameter.

(hear
 (let ((f (fdiv sample-rate 2))
       (x (mouse-x kr 0.9 4 0 0.1)))
   (mul (standard-l ar f x 0.5 0) 0.1)))

;; As a frequency control

(hear
 (let* ((x (mouse-x kr 0.9 4 0 0.1))
        (f (mul-add (standard-l ar 40 x 0.5 0) 800 900)))
   (mul (sin-osc ar f 0) 0.1)))
