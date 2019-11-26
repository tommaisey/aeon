;; (soft-clip a)

;; Nonlinear distortion.  Distortion with a perfectly linear region
;; from -0.5 to +0.5.

(audition 
 (out 0 (mul (soft-clip (mul (f-sin-osc ar 500 0.0)
			     (x-line kr 0.1 10 10 do-nothing)))
	     0.25)))
