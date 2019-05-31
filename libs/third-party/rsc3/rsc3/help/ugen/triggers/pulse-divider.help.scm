;; (pulse-divider trig div start)

;; outputs one impulse each time it receives a certain number of
;; triggers at its input.  A trigger happens when the signal changes
;; from non-positive to positive.

(let* ((p (impulse ar 8 0))
       (d (pulse-divider p (mce2 4 7) 0))
       (a (mul (sin-osc ar 1200 0) (decay2 p 0.005 0.1)))
       (b (mul (sin-osc ar 600 0)  (decay2 d 0.005 0.5))))
  (audition (out 0 (mul (add a b) 0.4))))
