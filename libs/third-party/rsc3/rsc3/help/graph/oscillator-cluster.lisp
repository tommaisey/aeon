;; oscillator cluster (rd)

(define ln
  (lambda (a b d)
    (line kr a b d remove-synth)))

(define xln
  (lambda (a b d)
    (x-line kr a b d remove-synth)))

(define rln
  (lambda (r a b d)
    (line kr (add a (rand 0 r)) b d remove-synth)))

(define rxln
  (lambda (r a b d)
    (x-line kr (add a (rand 0 r)) b d remove-synth)))

(define prt
  (lambda (d a)
    (lambda (cf)
      (let* ((r1 (rand cf (add cf 2)))
             (r2 (rln 1 5 0.01 d))
             (r3 (rln 10 20 0 d))
             (r4 (rand 0.1 0.2))
             (f (add (mce2 cf r1) (mul (sin-osc kr r2 0) r3)))
             (o (f-sin-osc ar f 0))
		       (e (mul (decay2 (impulse ar 0 0) r4 d) a)))
        (mul o e)))))

(define oscillator-cluster
  (let* ((np 12)
         (fp (replicate-m np (rand 220 660)))
         (d (rand 4 7))
         (a (rand 0.01 0.05)))
    (foldl add 0 (map (prt d a) fp))))

(hear oscillator-cluster)
