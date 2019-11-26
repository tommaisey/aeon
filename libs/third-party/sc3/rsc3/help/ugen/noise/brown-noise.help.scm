;; (brown-noise rate)

;; Generates noise whose spectrum falls off in power by 6 dB per
;; octave.

(audition (out 0 (mul (brown-noise ar) 0.1)))
