;; mouse clatter (rd)

(define mouse-clatter
  (let* ((x (mouse-x kr 100 12000 0 0.1))
         (y (mouse-y kr 0.01 0.15 0 0.1))
         (n1 (lf-noise0 kr (mce2 3 3.25)))
         (t (impulse kr (mul-add n1 16 18) 0))
         (n2 (t-rand 0.005 y t))
         (n3 (white-noise ar))
         (n4 (t-rand 10 x t))
         (n5 (t-rand 0.0 1.0 t))
         (n6 (t-exp-rand 0.15 1.0 t))
         (e (decay2 t 0.01 n2))
         (o (bpf (mul n3 e) n4 n5))
         (n7 (pv-rand-comb (fft* 10 o) n6 t)))
    (add (mul o 0.05) (ifft* n7))))

(with-sc3
 (lambda (fd)
   (begin
     (async fd (b-alloc 10 2048 1))
     (play fd (out 0 mouse-clatter)))))
