;; uplink (jmcc) #2

(define uplink
  (let* ((r (lambda (n) (rand 0 n)))
         (p0 (lf-pulse kr (r 20) 0 (r 1)))
         (p1 (mul-add (lf-pulse kr (r 4) 0 (r 1)) (r 8000) (r 2000)))
         (f (mix (clone 2 (mul p0 p1)))))
    (pan2 (mul (lf-pulse ar f 0 0.5) 0.04) (rand -0.8 0.8) 1)))

(with-sc3 (overlap-texture-u (list 4 1 5 +inf.0) uplink))
