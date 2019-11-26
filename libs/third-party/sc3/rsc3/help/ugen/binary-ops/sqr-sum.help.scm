;; (sqr-sum a b)

;; Square of the difference.  Return the value of (a + b)**2. This is
;; more efficient than using separate unit generators for each
;; operation.

(audition
 (out 0 (mul (sqr-sum (f-sin-osc ar 800 0)
		      (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0))
	     0.125)))

(let ((a (f-sin-osc ar 800 0))
      (b (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0)))
  (audition
   (out 0 (mul (mul (add a b) (add a b))
	       0.125))))
   