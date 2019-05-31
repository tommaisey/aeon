;; (lin-rand lo hi minmax)
 
;; Generates a single random float value in linear distribution from
;; lo to hi, skewed towards lo if minmax < 0, otherwise skewed towards
;; hi.

(let ((f (lin-rand 200 10000 (mce2 -1 1)))
      (a (line kr 0.4 0 0.01 remove-synth)))
  (audition (out 0 (mul (f-sin-osc ar f 0) a))))
