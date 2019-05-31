(import (rsc3))

(audition (out 0 (mul (blip ar 440 200) 0.15)))

;; Modulate frequency
(let ((f (x-line kr 20000 200 6 remove-synth)))
  (audition (out 0 (mul (blip ar f 100) 0.2))))

;; Modulate number of harmonics.
(let ((h (line kr 1 100 20 remove-synth)))
  (audition (out 0 (mul (blip ar 200 h) 0.2))))
