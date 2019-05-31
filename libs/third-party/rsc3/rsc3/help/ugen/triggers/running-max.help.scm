;; (running-max in trig)

;; Track maximum level.  outputs the maximum value received at the
;; input.  When triggered, the maximum output value is reset to the
;; current value.

;; in   - input signal
;; trig - reset the output value to the current input value

(let* ((t (impulse ar 0.4 0))
       (f (mul-add (running-max (dust ar 20) t) 500 200)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))

(let* ((t (impulse kr (mouse-x kr 0.01 2 1 0.1) 0))
       (f (mul-add (running-max (sin-osc kr 2 0) t) 500 200)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
