;; (u:sqrt a)

;; Square root.  The definition of square root is extended for signals
;; so that sqrt(a) when a<0 returns -sqrt(-a).

(let* ((a (line ar -2 2 3 remove-synth))
       (b (u:sqrt a))
       (f (mul-add (mce2 a b) 600 900)))
  (audition 
   (out 0 (mul (sin-osc ar f 0) 0.1))))
