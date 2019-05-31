;; (sub a b)

;; subtraction, written '-' in sclang.

;; silence

(let ((z (f-sin-osc ar 800 0)))
  (audition
   (out 0 (sub z z))))

;; non-silence

(audition (out 0 (mul (sub (white-noise ar) (white-noise ar)) 0.1)))
