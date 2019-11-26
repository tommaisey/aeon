;; clustered sines (jmcc) #2

(define clustered-sines
  (let* ((n 80)
         (f1 (rand 100 1100))
         (f2 (mul 4 f1))
         (y (replicate-m n (add f1 (rand 0 f2))))
         (z (klang-data y (map (lambda (e) (fdiv f1 e)) y) (replicate n 0)))
         (k (clone 2 (klang ar 1 0 z))))
    (mul k (fdiv 0.3 n))))

(with-sc3 (xfade-texture-u (list 4 4 +inf.0) clustered-sines))
