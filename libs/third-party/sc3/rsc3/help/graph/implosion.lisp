;; implosion (rd)

(define implosion
  (let* ((mkls
          (lambda (bp t)
            (env-gen kr 1 1 0 1 remove-synth (env-bp-linear bp t 1))))
         (mkrmp
          (lambda (l r t)
            (mkls (list 0 l 1 r) t)))
         (wrp
          (lambda (i l r)
            (let ((m (fdiv (sub r l) 2)))
              (mul-add i m (add l m)))))
         (pmr/n
          (lambda (rt l0 l1 r0 r1 d)
            (let ((le (mkrmp l0 r0 d))
                  (re (mkrmp l1 r1 d)))
              (wrp (white-noise rt) le re))))
         (d (rand 7.5 13.5))
         (f0 (rand 10990 16220))
         (f1 (rand  9440 19550))
         (f (pmr/n ar 440 f0 f1 f1 d))
         (l (pmr/n kr (rand -1 0) (rand 0 1) 0 0 d))
         (a (pmr/n kr 0.1 0.6 0 0 d)))
    (pan2 (saw ar f) l a)))

(hear implosion)
