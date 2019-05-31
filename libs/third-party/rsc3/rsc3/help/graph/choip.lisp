;; choip (jmcc) #10

(define xl (lambda (l r t) (x-line kr (exp-rand l r) (exp-rand l r) t do-nothing)))

(define r2 (lambda (n) (rand (neg n) n)))

(define choip
  (let* ((t 12)
         (i (impulse ar (xl 1 30 t) 0))
         (f (xl 600 8000 t))
         (a (sin-osc ar (add (mul3 (decay2 i 0.05 0.5) -0.9 f) f) 0))
         (l (line kr (r2 1) (r2 1) t do-nothing)))
    (pan2 (mul (decay2 (mul i (xl 0.01 0.5 t)) 0.01 0.2) a) l 1)))

(define choip-pp
  (let ((f (lambda (x) (allpass-n x 0.1 (clone 2 (rand 0 0.05)) 4))))
    (foldl1 compose (replicate 4 f))))

(with-sc3*
 (list
  (post-process-u 2 choip-pp)
  (overlap-texture-u (list 10 1 8 +inf.0) choip)))
