;; (scale-neg a b)

;; Scale negative part of input wave.  a * b when a < 0, otherwise a.

(audition
 (out 0 (scale-neg (f-sin-osc ar 500 0) (line ar 1 -1 4 remove-synth))))

