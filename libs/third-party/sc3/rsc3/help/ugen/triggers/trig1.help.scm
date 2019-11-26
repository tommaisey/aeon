;; (trig1 in dur)

;; When `in' is trigered output a unit signal for `dur' seconds.

(let ((a (trig1 (dust ar 1) 0.2)))
  (audition (out 0 (mul3 (f-sin-osc ar 800 0) a 0.2))))
