;; (db-amp a)

;; Convert decibels to linear amplitude.

(audition 
   (out 0 (mul (f-sin-osc ar 800 0.0)
	       (db-amp (line kr -3 -40 10 remove-synth)))))
