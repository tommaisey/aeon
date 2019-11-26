;; (lag-in num-channels bus lag)

;; Set bus 10.

(with-sc3 
 (lambda (fd) 
   (send fd (c-set1 10 (random 200 8000)))))

;; Smooth a control rate input signal.

(audition (out 0 (mul (sin-osc ar (lag-in 1 10 1) 0) 0.1)))
