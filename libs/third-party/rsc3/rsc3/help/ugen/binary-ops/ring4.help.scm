;; (ring4 a b)

;; Ring modulation variant.  Return the value of ((a*a *b) -
;; (a*b*b)). This is more efficient than using separate unit
;; generators for the multiplies.

;; See also mul, ring1, ring2, ring3.

(audition
 (out 0 (mul (ring4 (f-sin-osc ar 800 0)
		    (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0))
	     0.125)))

(let ((a (f-sin-osc ar 800 0))
      (b (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0)))
  (audition
   (out 0 (mul (sub (mul3 a a b) (mul3 a b b)) 
	       0.125))))
