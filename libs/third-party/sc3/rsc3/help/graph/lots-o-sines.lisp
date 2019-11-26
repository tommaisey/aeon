;; lots-o-sines (jmcc) #2

(define lots-o-sines
  (let* ((n 60)
         (z (klang-data (replicate-m n (lin-rand 40 10040 0))
                        (replicate n 1)
                        (replicate n 0)))
         (k (clone 2 (klang ar 1 0 z))))
    (mul k (fdiv 0.1 n))))

(with-sc3 (xfade-texture-u (list 4 4 +inf.0) lots-o-sines))
