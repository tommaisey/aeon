;; sclang default values

(hear
 (mul (fb-sine-c ar (fdiv sample-rate 4) 1 0.1 1.1 0.5 0.1 0.1) 0.2))

;; increase feedback

(hear
 (let ((fb (line kr 0.01 4 10 do-nothing)))
   (mul (fb-sine-c ar sample-rate 1 fb 1.1 0.5 0.1 0.1) 0.2)))

;; increase phase multiplier

(hear
 (let ((a (line kr 1 2 10 do-nothing)))
   (mul (fb-sine-c ar sample-rate 1 0 a 0.5 0.1 0.1) 0.2)))

;; randomly modulate parameters

(hear
 (let* ((x (mouse-x kr 1 12 0 0.1))
        (f (lambda (m a) (mul-add (lf-noise2 kr x) m a))))
   (mul (fb-sine-c ar
                   (f 1e4 1e4)
                   (f 32 33)
                   (f 0.5 0)
                   (f 0.05 1.05)
                   (f 0.3 0.3)
                   0.1
                   0.1)
        0.2)))
