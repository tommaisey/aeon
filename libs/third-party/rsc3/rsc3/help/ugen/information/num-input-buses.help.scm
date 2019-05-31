;; num-input-buses

;; The number of audio buses allocated to input.  input buses follow
;; output buses which begin at zero.

(let ((bus (add num-output-buses num-input-buses)))
  (audition (mrg2 (out 0 (in 1 ar bus))
		  (out bus (mul (sin-osc ar 440 0) 0.1)))))
