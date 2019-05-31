;; babbling brook (jmcc) #SC3

(define babbling-brook
  (let* ((b (lambda (f m a g)
              (let* ((n1 (brown-noise ar))
                     (n2 (brown-noise ar))
                     (n3 (mul-add (lpf n2 f) m a)))
                (mul (rhpf (one-pole n1 0.99) n3 0.03) g))))
         (x (clone 2 (b 14 400 500 0.024)))
         (y (clone 2 (b 20 800 1000 0.040))))
    (add x y)))

(hear babbling-brook)
