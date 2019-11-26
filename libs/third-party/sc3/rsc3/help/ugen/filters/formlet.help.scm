;; (formlet in freq attackTime decayTime) 

;; FOF-like filter

(let ((i (impulse ar 20 0.5)))
  (audition (out 0 (formlet i 1000 0.01 0.1))))

(let* ((f (x-line kr 10 400 8 remove-synth))
       (i (mul (blip ar f 1000) 0.1)))
  (audition (out 0 (formlet i 1000 0.01 0.1))))

;; Modulating formant frequency.

(let ((i (mul (blip ar (mul-add (sin-osc kr 5 0) 20 300) 1000) 0.1))
      (f (x-line kr 1500 700 8 remove-synth)))
  (audition (out 0 (formlet i f 0.005 0.04))))
