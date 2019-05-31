;; (amp-db a)

;; Convert linear amplitude to decibels.

(audition 
 (out 0 (mul (f-sin-osc ar 800 0.0)
	     (db-amp (amp-db (line kr 0.5 0.0 5 remove-synth))))))

(let* ((x (mouse-x kr -60 0 0 0.1))
       (f (mul-add (db-amp x) 600 900)))
  (audition 
   (out 0 (mul (sin-osc ar f 0) 0.1))))
