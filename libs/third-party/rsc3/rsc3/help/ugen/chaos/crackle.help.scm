(audition (out 0 (mul (crackle ar 1.95) 0.5)))

;; Modulate chaos parameter

(let ((p (line kr 1.0 2.0 3 remove-synth)))
  (audition (out 0 (mul (crackle ar p) 0.5))))
