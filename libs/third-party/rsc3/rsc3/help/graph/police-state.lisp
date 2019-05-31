;; police state (jmcc) #2

(define node
  (lambda (_)
    (let* ((r0 (rand 0.02 0.1))
           (r1 (rand 0 (* pi 2)))
           (r2 (rand 0 600))
           (r3 (rand 700 1300))
           (r4 (rand -1 1))
           (r5 (rand 80 120))
           (n0 (lf-noise2 ar r5))
           (f (mul-add (sin-osc kr r0 r1) r2 r3)))
      (pan2 (mul (sin-osc ar f 0) n0) r4 0.1))))

(define police-state
  (let* ((n0 (lf-noise2 kr (mce2 0.4 0.4)))
         (n1 (lf-noise2 ar (mul-add n0 90 620)))
         (n2 (lf-noise2 kr (mce2 0.3 0.3)))
         (e (mul n1 (mul-add n2 0.15 0.18))))
    (comb-l (add (mix-fill 4 node) e) 0.3 0.3 3)))

(hear police-state)
