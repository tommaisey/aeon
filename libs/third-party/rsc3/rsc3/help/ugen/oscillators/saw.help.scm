;; Band limited sawtooth wave generator.

(let ((f (x-line kr 40 4000 6 do-nothing)))
  (mul (saw ar f) 0.2))

;; Two band limited sawtooth waves thru a resonant low pass filter

(let ((f (x-line kr 8000 400 5 do-nothing)))
  (rlpf (mul (saw ar (mce2 100 250)) 0.1) f 0.05))
