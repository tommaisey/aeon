;; Modulate frequency

(hear
 (let ((f (x-line kr 40 4000 6 remove-synth)))
   (mul (pulse ar f 0.1) 0.2)))

;; modulate pulse width

(hear
 (let ((w (line kr 0.01 0.99 8 remove-synth)))
   (mul (pulse ar 200 w) 0.2)))

;; two band limited square waves thru a resonant low pass filter

(hear
 (rlpf (mul (pulse ar (mce2 100 250) 0.5) 0.1)
       (x-line kr 8000 40 5 remove-synth)
       0.05))
