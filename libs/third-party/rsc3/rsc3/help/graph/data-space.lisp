;; data space (jmcc) #2

(define data-space
  (let* ((r (lambda (n) (rand 0 n)))
         (lp (lambda (f m a) (mul-add (lf-pulse kr (r f) 0 (r 1)) m a)))
         (p0 (lp 200 1 0))
         (p1 (lp 40 (r 8000) (r 2000)))
         (p2 (lp 20 1 0))
         (p3 (lp 4 (r 8000) (r 2000)))
         (p4 (lp 20 1 0))
         (p5 (lp 4 (r 8000) (r 2000)))
         (f (add3 (mul p0 p1) (mul p2 p3) (mul p4 p5)))
         (dt (rand 0.15 0.35))
         (o (mul (lf-pulse ar f 0 0.5) 0.04))
         (l (mul (lf-noise0 kr (r 3)) 0.8)))
    (comb-l (pan2 o l 1) dt dt 3)))

(with-sc3 (overlap-texture-u (list 1 6 4 +inf.0) data-space))
