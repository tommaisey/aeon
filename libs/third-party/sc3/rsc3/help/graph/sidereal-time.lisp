;; sidereal time (jmcc) #9

(define sidereal-time
  (let* ((p 15)
	 (z (let ((y (let ((fr (clone p (exp-rand 100 6000)))
			   (rt (clone p (rand 2 6))))
		       (klank-data-mce fr (clone p 1) rt))))
	      (clone 2 y)))
	 (f (x-line kr (exp-rand 40 300) (exp-rand 40 300) 12 do-nothing))
	 (t (let ((e (lf-noise2 kr (rand 0 8))))
	      (mul (mul (lf-pulse ar f 0 (rand 0.1 0.9)) 0.002) (u:max 0 e))))
	 (o (mul (distort (klank t 1 0 1 (mce-transpose z))) 0.1)))
    (add (comb-n o 0.6 (rand 0.1 0.6) 8) (mce-reverse o))))

(with-sc3 (overlap-texture-u (list 4 4 6 dinf) sidereal-time))
