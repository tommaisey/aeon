;; (min a b)

;; Minimum.

(audition
 (out 0 (let ((z (f-sin-osc ar 500 0)))
	  (u:min z (f-sin-osc ar 0.1 0)))))
