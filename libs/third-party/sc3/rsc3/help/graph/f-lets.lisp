;; f-lets (rd)

(define f
  (lambda (t g j n f)
    (let ((pd (pulse-divider t j 0)))
      (mul (formlet pd
                    (mul f (ti-rand (mce2 2 1) n pd))
                    (t-rand 0.01 0.04 pd)
                    (t-rand 0.05 0.10 pd))
           g))))

(define n
  (lambda (t)
    (list (f t 0.15 2 9 (mce2 200 400))
          (f t 0.25 2 9 (mce2 (add 200 (t-rand 0 1 t))
                              (add 400 (t-rand 0 1 t))))
          (f t 0.05 4 5 (mce2 25 50))
          (f t 0.15 4 5 (mce2 (add 25 (t-rand 0 1 t))
                              (add 50 (t-rand 0 1 t))))
          (mul (f t 0.5 1 16 (mce2 300 600))
               (latch (coin-gate 0.2 t) t)))))

(define f-lets
  (mul (mul-add (lf-noise0 kr 2) 0.25 0.25)
       (foldl1 add (n (impulse ar 24 0)))))

(hear f-lets)
