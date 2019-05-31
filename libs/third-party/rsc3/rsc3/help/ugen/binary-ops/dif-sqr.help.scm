;; (dif-sqr a b)

;; Difference of squares.  Return the value of (a*a) - (b*b). This is
;; more efficient than using separate unit generators for each
;; operation.

(audition
   (out 0 (mul (dif-sqr (f-sin-osc ar 800 0)
			(f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0))
	       0.125)))

(audition
   (out 0 (let ((a (f-sin-osc ar 800 0))
		(b (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0)))
	    (mul (sub (mul a a) (mul b b)) 0.125))))
