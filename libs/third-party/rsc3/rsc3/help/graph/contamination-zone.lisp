(define cz
  (let ((f (exp-rand 800 8000))
        (p (let ((e (+ (* (lf-noise1 kr (rand 0 3)) 0.0008) 0.0022))) (* (pink-noise ar) e)))
        (s (+ (* (sin-osc kr (lin-rand 0 1 0) 0) (* 0.7 f)) f))
        (k (let ((sp (klank-data-mce (rand-n 4 50 2000) (mce (list 1 1 1 1)) (rand-n 4 0.2 4))))
             (* (abs (klank p 1 0 1 sp)) (u-choose (mce2 -1 1)))))
        (r (rlpf k s 0.1))
        (a (lf-pulse kr (lin-rand 0 150 0) 0 (rand 0.2 0.4))))
    (pan2 r (lf-noise1 kr (rand 0 1)) a)))

(define cz-pp
  (let ((f (lambda (x) (allpass-n x 0.04 (rand-n 2 0 0.04) 16))))
    (useq 6 f)))

(with-sc3*
 (list
  (post-process-u 2 cz-pp)
  (overlap-texture-u (list 3 8 4 inf) cz)))

