;; (n-rand lo hi n)
 
;; Generates a single random float value in a sum of `n' uniform
;; distributions from `lo' to `hi'.
 
;; n = 1 : uniform distribution - same as rand
;; n = 2 : triangular distribution
;; n = 3 : smooth hump
;; as n increases, distribution converges towards gaussian

(let ((f (mul (n-rand 1200 4000 2) (mce2 2 5)))
      (a (line kr 0.2 0 0.01 remove-synth)))
  (audition (out 0 (mul (f-sin-osc ar f 0) a))))
