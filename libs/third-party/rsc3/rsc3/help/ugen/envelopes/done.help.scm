;; (done src)

;; outputs a unit signal if the 'done' flag of the unit at `src' is
;; set, else output zero.

(let* ((x (mouse-x kr -1 1 0 0.1))
       (e (linen x 0.1 0.1 0.5 do-nothing))
       (l (mul (sin-osc ar 880 0) 0.1))
       (r (sin-osc ar 440 0)))
  (audition (out 0 (mce2 (mul (done e) l)
			 (mul e r)))))
