;; (dust rate density)

;; Generates random impulses from 0 to +1 at a rate determined by the
;; density argument.

(audition (out 0 (mul (dust ar 200) 0.5)))

(let ((r (x-line kr 20000 2 10 remove-synth)))
  (audition (out 0 (mul (dust ar r) 0.5))))
