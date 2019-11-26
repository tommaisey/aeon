;; (i-rand lo hi)

;; Generates a single random integer value in uniform distribution
;; from `lo' to `hi'.

(let ((f (i-rand 200 1200))
      (a (line kr 0.2 0 0.1 remove-synth)))
  (audition (out 0 (mul (f-sin-osc ar f 0) a))))
