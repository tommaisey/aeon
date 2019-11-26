;; (sign a)

;; Sign function.  -1 when a < 0, +1 when a > 0, 0 when a is 0

(let* ((a (line ar -1 1 1 remove-synth))
       (b (sign a))
       (f (mul-add (mce2 a b) 600 900)))
  (audition 
   (out 0 (mul (sin-osc ar f 0) 0.1))))
