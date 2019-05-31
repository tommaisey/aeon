;; (running-min in trig)

;; Track minimum level.  outputs the minimum value received at the
;; input.  When triggered, the minimum output value is reset to the
;; current value.

;; in   - input signal
;; trig - reset the output value to the current input value

(let* ((t (impulse ar 2.0 0))
       (f (mul-add (running-min (sub 1 (dust ar 20)) t) 500 200)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))

(let* ((t (impulse kr (mouse-x kr 0.5 4 1 0.1) 0))
       (f (mul-add (running-min (sub 2 (sin-osc kr 2 0)) t) 500 200)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
