;; (set-reset-ff trig reset)

;; Set-reset flip flop.  output is set to 1.0 upon receiving a trigger
;; in the set input, and to 0.0 upon receiving a trigger in the reset
;; input. Once the flip flop is set to zero or one further triggers in
;; the same input are have no effect. One use of this is to have some
;; precipitating event cause something to happen until you reset it.

;; trig  - trigger sets output to one
;; reset - trigger resets output to zero

(let ((n (brown-noise ar))
      (g (set-reset-ff (dust ar 5) (dust ar 5))))
  (audition (out 0 (mul3 n g 0.2))))
