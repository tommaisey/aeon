;; (pitch-shift in winSize pchRatio pchDispersion timeDispersion)

;; A simple time domain pitch shifter.

(audition
 (out 0 (pitch-shift (sin-osc ar 440 0) 
		     0.2
		     (mouse-x kr 0.5 2 0 0.1)
		     (mouse-y kr 0 0.1 0 0.1)
		     0)))
