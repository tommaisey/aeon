(define saucer-base
  (let* ((b (rand 0 1000))
	 (c (rand 0 5000))
	 (o (add (mul (sin-osc ar (rand 0 20) 0) b) (mul 1.1 b)))
	 (p (add (mul (sin-osc ar o 0) c) (mul 1.1 c))))
    (pan2 (mul (sin-osc ar p 0) 0.1) (rand -1 1) 1)))

(with-sc3 (overlap-texture-u (list 2 6 4 dinf) saucer-base))
