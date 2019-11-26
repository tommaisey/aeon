;; (schmidt in lo hi)

;; schmidt trigger.  When in crosses to greater than hi, output 1.0,
;; then when signal crosses lower than lo output 0.0. output is
;; initially zero.

;; in - signal to be tested
;; lo - low threshold
;; hi - high threshold

(let* ((in (lf-noise1 kr 3))
       (octave (add (schmidt in -0.15 0.15) 1))
       (f (add (mul in 200) (mul 500 octave))))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
