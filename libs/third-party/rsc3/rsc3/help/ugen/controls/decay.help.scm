;; Used as an envelope.

(hear
 (mul (decay (impulse ar (x-line kr 1 50 20 remove-synth) 0.25) 0.2)
      (pink-noise ar)))
