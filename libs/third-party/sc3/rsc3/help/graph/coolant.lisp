;; coolant (jmcc) #2

(define coolant
  (let* ((p 10)
         (n (replicate p 1))
         (sp (mce2 (klank-data (replicate-m p (rand 40 2400)) n n)
                   (klank-data (replicate-m p (rand 40 2400)) n n)))
         (s (one-pole (mul (clone 2 (brown-noise ar)) 0.002) 0.95)))
    (klank s 1 0 1 (mce-transpose sp))))

(with-sc3 (overlap-texture-u (list 4 4 2 +inf.0) coolant))
