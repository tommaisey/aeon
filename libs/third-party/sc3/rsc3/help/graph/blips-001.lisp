;; blips 001 (jmcc) #SC3d1.5

(define blips-001
  (let* ((c (coin 0.8))
         (b (lambda ()
              (let ((f (x-line kr (exp-rand 0.25 400) (exp-rand 0.25 400) 4 0))
                    (nh (x-line kr (exp-rand 2 100) (exp-rand 2 100) 4 0)))
                (blip ar f nh)))))
    (mul c (pan2 (mul (b) (b)) (line kr (rand2 1) (rand2 1) 4 0) 0.3))))

(define blips-pp
  (lambda (z)
    (let ((f (lambda (x) (allpass-n x 0.05 (mce2 (rand 0 0.05) (rand 0 0.05)) 4))))
      (iterate 6 f (distort z)))))

(with-sc3*
 (list
  (post-process-u 2 blips-pp)
  (overlap-texture-u (list 2 1 12 +inf.0) blips-001)))
