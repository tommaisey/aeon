;; (pulse-count trig reset)

;; This outputs the number of pulses received at `trig' and outputs
;; that value until `reset' is triggered.

(let ((f (mul (pulse-count (impulse ar 10 0) (impulse ar 0.4 0)) 200)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.05))))
