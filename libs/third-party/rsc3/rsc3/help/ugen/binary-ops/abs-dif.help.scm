;; (abs-dif a b)

;; Calculates the value of (abs (- a b). Finding the magnitude of the
;; difference of two values is a common operation.

(audition 
 (out 0 (mul (f-sin-osc ar 440 0)
	     (abs-dif 0.2 (mul (f-sin-osc ar 2 0) 0.5)))))
