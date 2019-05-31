;; bowed string (jmcc)

(define series
  (lambda (f n x i)
    (if (= n 0)
        nil
        (cons x (series f (- n 1) (f x i) i)))))

(define bowed-string
  (let* ((root 5)
         (scale (map (lambda (n) (+ n root)) (list 0 2 4 5 7 9 11)))
         (oct (list 24 36 48 60 72 84))
         (choose (lambda (l) (select (i-rand 0 (length l)) (make-mce l))))
         (f (midi-cps (add (choose scale) (choose oct))))
         (n0 (clone 2 (brown-noise ar)))
         (r0 (exp-rand 0.125 0.5))
         (n1 (lf-noise1 kr r0))
         (r1 (rand 0.7 0.9))
         (r2 (replicate-m 12 (rand 1.0 3.0)))
         (x (mul3 n0 0.007 (u:max 0 (mul-add n1 0.6 0.4))))
         (d (klank-data (series add 12 f f) (series mul 12 1 r1) r2))
         (k (klank x 1 0 1 d)))
    (soft-clip (mul k 0.1))))

(with-sc3 (overlap-texture-u (list 5 2 12 +inf.0) bowed-string))
