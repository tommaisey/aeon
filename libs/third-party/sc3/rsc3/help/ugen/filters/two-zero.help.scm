;; (two-zero in freq radius)

;; Two zero filter

(audition
 (out 0 (two-zero (mul (white-noise ar) 0.125) 
		  (x-line kr 20 20000 8 remove-synth) 
		  1)))
