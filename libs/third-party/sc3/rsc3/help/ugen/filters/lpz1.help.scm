;; (lpz1 ar in)

;; Two point average filter

(audition
 (out 0 (lpz1 (mul (white-noise ar) 0.25))))
