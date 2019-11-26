;; birdies (jmcc)

(define birdies
  (let* ((p1 (add (mul (lf-pulse kr (rand 0.4 1.4) 0 (rand 0.1 0.9)) (rand 4 7)) 2))
	 (p2 (mul (lf-pulse kr (rand 0.4 1.4) 0 (rand 0.1 0.9)) (rand 4 7)))
	 (p3 (mul (lf-pulse kr (rand 0.2 0.7) 0 0.4) 0.02))
	 (sw (add (mul (lf-saw kr (add p1 p2) 0) (neg (rand 1000 1800))) (add 4000 (rand2 1200))))
	 (freq (lag sw 0.05))
	 (amp (lag p3 0.3)))
    (pan2 (mul (sin-osc ar freq 0) amp) (rand2 1) 1)))

(with-sc3 (overlap-texture-u (list 7 4 4 dinf) birdies))
