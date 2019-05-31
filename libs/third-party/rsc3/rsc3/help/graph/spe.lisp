;; spe (jmcc)

(define chain-of
  (lambda (n f)
    (foldl1 compose (replicate n f))))

(define rapf
  (lambda (i)
    (allpass-n i 0.05 (clone 2 (rand 0 0.05)) 4)))

(define spe
  (let* ((n (lf-noise1 kr 1))
         (s (make-mce (list 00 03 02 07
                            08 32 16 18
                            00 12 24 32)))
         (m (dseq dinf s))
         (t (impulse kr 9 0))
         (f (midi-cps (add (demand t 0 m) 32)))
         (p (env-perc 0.01 1 1 (list -4 -4)))
         (e (env-gen kr t 0.1 0 1 do-nothing p))
         (o (mul (lf-saw ar f 0) e))
         (rq (midi-cps (mul-add n 36 110))))
    ((chain-of 4 rapf) (rlpf o rq 0.1))))

(hear spe)
