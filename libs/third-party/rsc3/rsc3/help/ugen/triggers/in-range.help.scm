;; (in-range in lo hi)

;; Tests if a signal is within a given range.

;; If in is >= lo and <= hi output 1.0, otherwise output 0.0. output
;; is initially zero.

;; in - signal to be tested
;; lo - low threshold
;; hi - high threshold

(let ((a (in-range (mul (sin-osc kr 1 0) 0.2) -0.15 0.15)))
  (audition (out 0 (mul a (mul (brown-noise ar) 0.1)))))
