;; (brz2 in)

;; A two zero fixed midcut filter.  A special case fixed
;; filter. Implements the formula:

;; out(i) = 0.5 * (in(i) + in(i-2))

;; This filter cuts out frequencies around 1/2 of the Nyquist
;; frequency.

;; Compare:

(audition (out 0 (mul (white-noise ar) 0.15)))

(audition (out 0 (brz2 (mul (white-noise ar) 0.15))))
