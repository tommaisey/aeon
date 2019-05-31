;; SC2: Note extra iphase argument.

(import (rsc3))

(audition (out 0 (mul (f-sin-osc ar (mce2 440 550) 0) 0.05)))

(let ((f (x-line kr 200 4000 1 remove-synth)))
  (audition (out 0 (mul (f-sin-osc ar f 0) 0.25))))

;; Loses amplitude towards the end
(let ((f (mul-add (f-sin-osc ar (x-line kr 4 401 8 remove-synth) 0)
                  200
                  800)))
  (audition (out 0 (mul (f-sin-osc ar f 0) 0.25))))
