(hear (mul (lf-noise0 ar 1000) 0.25))
(hear (mul (lf-noise1 ar 1000) 0.25))
(hear (mul (lf-noise2 ar 1000) 0.25))

;; Modulate frequency.

(hear (mul (lf-noise0 ar (x-line kr 1000 10000 10 remove-synth)) 0.25))
(hear (mul (lf-noise1 ar (x-line kr 1000 10000 10 remove-synth)) 0.25))
(hear (mul (lf-noise2 ar (x-line kr 1000 10000 10 remove-synth)) 0.25))

;; Use as frequency control.

(hear (mul (sin-osc ar (mul-add (lf-noise0 kr 4) 400 450) 0) 0.2))
(hear (mul (sin-osc ar (mul-add (lf-noise1 kr 4) 400 450) 0) 0.2))
(hear (mul (sin-osc ar (mul-add (lf-noise2 kr 4) 400 450) 0) 0.2))
