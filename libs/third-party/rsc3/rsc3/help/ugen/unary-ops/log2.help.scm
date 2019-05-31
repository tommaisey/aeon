;; (log2 a)

;; Base two logarithm.

(let* ((a (line ar -2 2 3 remove-synth))
       (b (log2 a))
       (f (mul-add (mce2 a b) 600 900)))
  (audition 
   (out 0 (mul (sin-osc ar f 0) 0.1))))
