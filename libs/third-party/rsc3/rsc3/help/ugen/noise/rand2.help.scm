;; (rand2 n)

(import (rsc3))

(let* ((a (line kr 0.2 0 0.1 remove-synth))
       (p (rand2 1))
       (s (mul (f-sin-osc ar (rand 200 1200) 0) a)))
  (audition (out 0 (pan2 s p 1))))
