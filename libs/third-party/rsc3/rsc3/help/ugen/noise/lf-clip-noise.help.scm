;; (lfclip-noise rate freq)

;; randomly generates the values -1 or +1 at a rate given by the
;; nearest integer division of the sample rate by the freq argument.
;; It is probably pretty hard on your speakers.  The freq argument is
;; the approximate rate at which to generate random values.
 
(audition (out 0 (mul (lfclip-noise ar 1000) 0.1)))

;; Modulate frequency 

(let ((f (x-line kr 1000 10000 10 remove-synth)))
  (audition (out 0 (mul (lfclip-noise ar f) 0.1))))

;; Use as frequency control 

(let ((f (mul-add (lfclip-noise kr 4) 200 600)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
