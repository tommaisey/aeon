;; deep-trip (jmcc) #9

(define deep-trip
  (let* ((f (midi-cps (add (mul (lf-noise1 kr (rand 0 0.3)) 60) 70)))
	 (a2 (mul (sin-osc kr (rand 0 40) 0) 0.1))
	 (a1 (u:max 0 (mul (lf-noise1 kr (rand 0 8)) a2)))
	 (a (mul (lf-noise2 ar (mul f (rand 0 0.5))) a1))
	 (z (mul (sin-osc ar f 0) a))
	 (s (pan2 z (lf-noise1 kr (rand 0 5)) 1))
	 (c0 (comb-n s 0.5 (rand-n 2 0.3 0.5) 20))
	 (c1 (comb-n s 0.5 (rand-n 2 0.3 0.5) 20)))
    (add (add s c0) c1)))

(with-sc3 (overlap-texture-u (list 4 12 4 dinf) deep-trip))
