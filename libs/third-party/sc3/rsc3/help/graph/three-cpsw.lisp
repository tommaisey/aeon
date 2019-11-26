;; three-cpsw (rd)

(define three-cpsw
  (let* ((t (dust kr (mce2 12 18)))
         (f0 (t-rand 1 64 t))
         (f1 (lf-noise0 kr f0))
         (a  (t-rand 0.0 0.5 t))
         (dt (t-rand 0.975 1.025 t))
         (dh (t-rand 0.750 0.7505 t))
         (f (mul-add f1 (mce2 9000 12000) 9500))
         (o (add3 (saw ar f) (saw ar (mul f dh)) (saw ar (mul f dt)))))
    (clip2 (mul o a) 0.75)))

(hear three-cpsw)
