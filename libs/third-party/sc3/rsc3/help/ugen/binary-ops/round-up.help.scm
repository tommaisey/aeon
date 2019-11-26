;; (round-up a b)

;; Rounds a up to the nearest multiple of b.

(let* ((x (mouse-x kr 60 4000 0 0.1))
       (f (round-up x 100)))
  (audition
   (out 0 (mul (sin-osc ar f 0) 0.1))))

(let ((n (line kr 24 108 6 remove-synth)))
  (audition
   (out 0 (mul (saw ar (midi-cps (round-up n 1))) 0.2))))
