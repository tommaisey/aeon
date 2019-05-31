;; Default initial parameters.

(hear
 (let ((x (mouse-x kr 20 sample-rate 0 0.1)))
   (mul (latoocarfian-c ar x 1 3 0.5 0.5 0.5 0.5) 0.2)))

;; randomly modulate all parameters.

(hear
 (mul (latoocarfian-c
       ar
       (fdiv sample-rate 4)
       (mul-add (lf-noise2 kr 2) 1.5 1.5)
       (mul-add (lf-noise2 kr 2) 1.5 1.5)
       (mul-add (lf-noise2 kr 2) 0.5 1.5)
       (mul-add (lf-noise2 kr 2) 0.5 1.5)
       0.5
       0.5)
      0.1))
