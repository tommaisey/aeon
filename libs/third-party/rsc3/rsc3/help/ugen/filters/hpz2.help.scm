;; (hpz2 in)

;; Two zero fixed highpass filter.

(audition (out 0 (hpz2 (mul (white-noise ar) 0.25))))
