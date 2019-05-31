;; Used as an envelope

(hear
 (mul3 (decay2 (impulse ar (x-line kr 1 50 20 remove-synth) 0.25) 0.01 0.2)
       (f-sin-osc ar 600 0)
       0.25))

;; Compare the above with decay used as the envelope.

(hear
 (mul3 (decay (impulse ar (x-line kr 1 50 20 remove-synth) 0.25) 0.01)
       (f-sin-osc ar 600 0)
       0.25))
