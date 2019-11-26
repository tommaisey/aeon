;; (one-pole in coef)

;; A one pole filter.  Implements the formula: out(i) = ((1 -
;; abs(coef)) * in(i)) + (coef * out(i-1)).

;; in   - input signal to be processed
;; coef - feedback coefficient. Should be between -1 and +1

(audition
 (out 0 (one-pole (mul (white-noise ar) 0.5) 0.95)))

(audition
 (out 0 (one-pole (mul (white-noise ar) 0.5) -0.95)))

(audition
 (out 0 (one-pole (mul (white-noise ar) 0.5) 
		  (line kr -0.99 0.99 10 remove-synth))))



