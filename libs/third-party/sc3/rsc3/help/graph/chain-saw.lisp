;; chain saw (jrhb)

;; SimpleNumber.coin (not UGen/Coin)
(define coin*
  (lambda (n a b)
    (if (> (random 0.0 1.0) n) a b)))

(define exp-range
  (lambda (s l r)
    (lin-exp s -1 1 l r)))

(define chain
  (lambda (n fn)
    (foldl1 compose (replicate n fn))))

(define mce-product
  (mce-edit (lambda (l) (list (foldl1 mul l)))))

(define clipu
  (lambda (s) (clip2 s 1)))

(define dup
  (lambda (a) (mce2 a a)))

(define mk-f
  (lambda (s1)
    (let* ((xr (clone 2 (exp-rand 0.1 2)))
           (n1 (lf-noise1 kr xr))
           (n2 (lf-noise1 kr xr))
           (n3 (lf-noise1 kr xr))
           (f1 (coin* 0.6 (exp-range n1 0.01 10) (exp-range n2 10 50)))
           (s2 (coin* 0.5 (sub 1 s1) (mce-reverse s1)))
           (f2 (lin-exp s1 -1 1 f1 (mul f1 (exp-range n3 2 10))))
           (u1 (lf-saw kr f2 0))
           (u2 (mul-add (lf-saw kr (mul f1 0.1) 0) 0.1 1)))
      (clipu (coin* 0.5 (mul u1 s2) (mul u1 u2))))))

(define chain-saw
  (let* ((inp (lf-saw kr (mul 0.2 (mce2 1 1.1)) 0))
         (b-freq (make-mce (list 70 800 9000 5242)))
         (ff ((chain 8 mk-f) inp))
         (c-saw (mce-product (saw ar (exp-range ff 6 11000))))
         (b-saw (dup (mix (bpf c-saw b-freq 0.2)))))
    (mul b-saw 0.3)))

(hear chain-saw)
