;; xy-interference (rd)

(define xy-interference
  (let* ((x (mouse-x kr 20 22000 1 (mce2 0.005 0.025)))
         (y (mouse-y kr 20 22000 1 (mce2 0.005 0.075)))
         (nd (lambda (_)
               (let* ((n (lf-noise0 kr (mce2 5 9)))
                      (a (sin-osc ar (add x n) 0))
                      (b (sin-osc ar y 0)))
                 (mul a b)))))
    (mix-fill 3 nd)))

(hear (mul xy-interference 0.1))
