;; (gate in trig)

;; The signal at `in' is passed while `trig' is greater than zero.

(let ((s (mul (f-sin-osc ar 500 0) 1/4))
      (t (lf-pulse ar 1 0 1/10)))
  (audition (out 0 (gate s t))))
