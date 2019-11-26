;; (clip2 a b)

;; Bilateral clipping.  clips a to +/- b

(audition
 (out 0 (clip2 (f-sin-osc ar 400 0) 0.2)))

(audition
 (out 0 (clip2 (f-sin-osc ar 400 0) (line kr 0 1 8 remove-synth))))
