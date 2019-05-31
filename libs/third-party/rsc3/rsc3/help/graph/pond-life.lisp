;; pond life (jmcc) #1

(define pond-life
  (let* ((f0 (add 20 (rand 0 30)))
         (f1 (mul-add (f-sin-osc kr f0 0) (rand 100 400) (lin-rand 500 2500 0)))
         (a (mul (lf-pulse kr (fdiv 3 (rand 1 9)) 0 (rand 0.2 0.5)) 0.04)))
    (pan2 (sin-osc ar f1 0) (rand -1 1) a)))

(with-sc3 (overlap-texture-u (list 8 8 4 +inf.0) pond-life))
