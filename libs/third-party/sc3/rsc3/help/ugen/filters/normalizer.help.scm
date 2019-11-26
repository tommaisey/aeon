;; (normalizer in level dur) 

;; Flattens dynamics.

(let ((z (mul (decay2 (impulse ar 8 (mul (lf-saw kr 0.25 -0.6) 0.7))
		      0.001  
		      0.3)
	      (f-sin-osc ar 500 0))))
  (audition (out 0 (mce2 z (normalizer z 0.4 0.01)))))
