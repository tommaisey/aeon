;; (free-self src)

;; free enclosing synth when the input signal `src' crosses from
;; non-positive to positive.

(audition 
 (mrg2 (free-self (mouse-x kr -1 1 0 0.1))
       (out 0 (mul (sin-osc ar 440 0) 0.1))))
