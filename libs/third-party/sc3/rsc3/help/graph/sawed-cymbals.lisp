(define sawed-cymbals
  (let* ((y (let* ((f1 (rand 500 2500))
		   (f2 (rand 0 8000))
		   (f (clone 15 (rand f1 (add f1 f2))))
		   (rt (clone 15 (rand 2 6))))
	      (klank-data-mce f (clone 15 1) rt)))
	 (z (clone 2 y))
	 (f-s (x-line kr (rand 0 600) (rand 0 600) 12 do-nothing)))
    (klank (mul (lf-saw ar f-s 0) 0.0005) 1 0 1 (mce-transpose z))))

(with-sc3 (overlap-texture-u (list 4 4 6 dinf) sawed-cymbals))
