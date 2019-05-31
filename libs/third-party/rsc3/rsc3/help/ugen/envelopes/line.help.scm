;; (line rate start end dur doneAction)

;; Generates a line from the start value to the end value.

;; start - starting value
;; end   - ending value
;; dur   - duration in seconds

;; Note: The SC3 UGen reorders the mul and add inputs to precede the
;; doneAction input.

(let ((f (line kr 200 17000 5 remove-synth)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
