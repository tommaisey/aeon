;; (max a b)

;; Maximum.

(audition
   (out 0 (let ((z (f-sin-osc ar 500 0)))
	    (u:max z (f-sin-osc ar 0.1 0)))))
