;; (rand lo hi)

;; Generates a single random value in uniform distribution from lo to
;; hi.  It generates this when the SynthDef first starts playing, and
;; remains fixed for the duration of the synth's existence.

(import (rsc3))

(let* ((a (line kr 0.2 0 0.1 remove-synth))
       (p (rand -1 1))
       (s (mul (f-sin-osc ar (rand 200 1200) 0) a)))
  (audition (out 0 (pan2 s p 1))))
