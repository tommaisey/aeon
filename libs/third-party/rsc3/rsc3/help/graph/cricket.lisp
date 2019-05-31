;; cricket (rd)

(define cricket
  (let* ((m-rand
          (lambda (l r)
            (mce2 (rand l r) (rand l r))))
         (mt-rand
          (lambda (l r t)
            (mce2 (t-rand l r t) (t-rand l r t))))
         (r1 (m-rand 10 13))
         (r2 (m-rand 10 13))
         (r3 (m-rand 4 7))
         (t (impulse kr 0.7 0))
         (e (decay2 (impulse kr r1 0) 0.001 0.005))
         (f (mul3 (sin-osc kr r2 0) e r3))
         (r4 (mt-rand 2220 2227 t)))
    (mul3 (sin-osc ar r4 0) f 0.25)))

(hear cricket)
