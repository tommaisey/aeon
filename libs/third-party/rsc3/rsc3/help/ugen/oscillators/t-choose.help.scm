(hear
 (let* ((t (dust ar (mouse-x kr 1 1000 1 0.1)))
	(f (midi-cps (ti-rand 48 60 t)))
	(a (mce3 (sin-osc ar f 0)
		 (saw ar (mul f 2))
		 (pulse ar (mul f 0.5) 0.1))))
   (mul (t-choose t a) 0.1)))
