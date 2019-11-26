;; (exp-rand lo hi)

;; Generates a single random float value in an exponential
;; distributions from `lo' to `hi'.

(let ((f (exp-rand 100 8000))
      (a (line kr 0.5 0 0.01 remove-synth)))
  (audition (out 0 (mul (f-sin-osc ar f 0) a))))
