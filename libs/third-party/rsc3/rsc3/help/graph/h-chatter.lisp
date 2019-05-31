;; h-chatter (rd)

(define wrp
  (lambda (i l r)
    (let ((m (fdiv (sub r l) 2)))
      (mul-add i m (add l m)))))

(define h0
  (let* ((n (mul-add (lf-noise0 kr 1) 5 5))
         (a (mul-add (lf-noise2 kr n) 0.20 1.20))
         (b (mul-add (lf-noise2 kr n) 0.15 0.15))
         (f 40)
         (h (henon-n ar (mce2 f (mul f 0.5)) a b 0 0)))
    (mul (saw ar (mul-add h 3200 1600)) 0.35)))

(define h1
  (let* ((n0 (lf-noise0 ar 32))
         (n1 (lf-noise0 ar 2))
         (a (mouse-x kr 1.2 1.4 0 0.1))
         (b (mouse-y kr 0.2 0.3 0 0.1))
         (h (wrp n0 1 32))
         (p (wrp n1 2400 3200))
         (l (wrp n1 -0.75 0.75))
         (g (wrp n1 0.55 0.85))
         (f 40)
         (o (blip ar (wrp (henon-n ar f a b 0 0) p (mul p 2)) h)))
    (mul (pan2 o l g) 0.35)))

(define h-chatter (add h0 h1))

(hear h-chatter)
