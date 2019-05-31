;; (t-exp-rand lo hi trig)

;; Generates a random float value in exponential distribution from lo
;; to hi each time the trig signal changes from nonpositive to
;; positive values lo and hi must both have the same sign and be
;; non-zero.

(let* ((t (dust kr 10))
       (f (t-exp-rand 300 3000 t)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
