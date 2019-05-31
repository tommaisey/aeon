;; hell is busy (jmcc) #1

(define hell-is-busy
  (let ((o (f-sin-osc ar (add 400 (rand 0 2000)) 0))
        (a (mul (lf-pulse kr (add 1 (rand 0 10.0)) 0 (rand 0 0.7)) 0.04)))
    (pan2 (mul o a) (rand -1 1) 1)))

(with-sc3 (overlap-texture-u (list 4 4 8 +inf.0) hell-is-busy))
