;; (ring1 a b)

;; Ring modulation plus first source.  Return the value of ((a*b) +
;; a). This is more efficient than using separate unit generators for
;; the multiply and add.

;; See also mul, Ring1, Ring2, Ring3, Ring4.

(audition
 (out 0 (mul (ring1 (f-sin-osc ar 800 0)
		    (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0))
	     0.125)))

(let ((a (f-sin-osc ar 800 0))
      (b (f-sin-osc ar (x-line kr 200 500 5 do-nothing) 0)))
  (audition
   (out 0 (mul (add (mul a b) a) 0.125))))
