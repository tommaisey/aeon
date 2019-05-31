;; scratchy (jmcc) #1

(define scratchy
  (let* ((n (mul (clone 2 (brown-noise ar)) 0.5))
         (f (mul (u:max (sub n 0.49) 0) 20)))
    (rhpf f 5000 1)))

(hear scratchy)
