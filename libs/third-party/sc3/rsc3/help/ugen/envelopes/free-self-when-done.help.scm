;; (free-self-when-done src)

;; free the synth when the 'done' flag of the unit at `src' is set.

(let* ((x (mouse-x kr -1 1 0 0.1))
       (e (linen x 1 0.1 1 remove-synth)))
  (audition (out 0 (mul (sin-osc ar 440 0) e))))

(let* ((x (mouse-x kr -1 1 0 0.1))
       (e (linen x 2 0.1 2 do-nothing)))
  (audition (mrg2 (free-self-when-done e)
		  (out 0 (mul (sin-osc ar 440 0) e)))))
