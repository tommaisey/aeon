;; (thresh a b)

;; Signal thresholding.  0 when a < b, otherwise a.

(audition
 (out 0 (thresh (mul (lf-noise0 ar 50) 0.5) 0.45)))
