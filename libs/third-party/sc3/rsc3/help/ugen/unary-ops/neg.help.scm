;; (neg a)

;; Negation.

(let ((s (sin-osc ar 440 0)))
  (audition 
   (out 0 (mce2 (mul s 0.1)
		(add s (neg s))))))
