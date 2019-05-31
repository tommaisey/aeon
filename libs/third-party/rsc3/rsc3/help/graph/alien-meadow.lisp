;; alien meadow (jmcc) #6

(define alien-meadow
  (let* ((a (rand 0 20))
         (b (rand 0 5000))
         (c (rand 0 20))
         (p (rand -1 1))
         (f (add (mul3 (sin-osc ar a 0) b 0.1) b)))
    (pan2 (sin-osc ar f 0) p (mul-add (sin-osc ar c 0) 0.05 0.05))))

(with-sc3 (overlap-texture-u (list 2 6 6 +inf.0) alien-meadow))
