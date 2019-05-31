;; k-ppr (rd)

(define wrp
  (lambda (i l r)
    (lin-lin i -1 1 l r)))

(define stream
  (lambda (y ti t lf rf ld rd g)
    (lambda (_)
      (let* ((r1 (rand 9 18))
             (t* (pulse-divider t r1 0))
             (r2 (t-rand lf (wrp ti lf rf) t*))
             (r3 (t-rand ld rd t*)))
        (mul (ringz (decay2 t* 0.01 0.5) r2 (mul r3 y)) g)))))

(define k-ppr
  (let* ((x (mouse-x kr 0.05 0.35 0 0.1))
         (y (mouse-y kr 0.15 0.75 0 0.1))
         (ti (lf-tri kr x 0))
         (tf (wrp ti 100 200))
         (t (impulse ar tf 0))
         (s1 (stream y ti t 3140 6240 0.050 0.005 0.15))
         (s2 (stream y ti t 0400 9000 0.005 0.005 0.15)))
    (add (mce-fill 2 s1) (mce-fill 2 s2))))

(hear k-ppr)
