;; rzblp (rd)

(define wrp
  (lambda (i l r)
    (let ((m (fdiv (sub r l) 2)))
      (mul-add i m (add l m)))))

(define lfn
  (lambda (f l r)
    (wrp (lf-noise0 kr f) l r)))

(define hpb
  (lambda (q)
    (mix-fill
     2
     (lambda (_)
       (let ((f (lfn q 1330 1395))
             (a (lfn q 0.001 0.007)))
         (mul (blip ar f 24) a))))))

(define f
  (lambda (u)
    (let* ((q (lfn 5.2 5.2 5.21))
           (a (u dinf (mce5 1 3 2 7 8)))
           (trig (impulse kr q 0))
           (freq (mul-add (demand trig 0 a) 30 340)))
      (foldl1 add
              (list (mul (blip ar freq 3)
                         (lfn q 0.001 0.01))
                    (mul (resonz (impulse ar q (mce2 0 0))
                                 (lfn 5 30 640)
                                 (lfn q 0.1 0.5))
                         (lfn q 0.01 1.8))
                    (mul (hpb q)
                         (lfn q 1.2 1.8))
                    (mul (blip ar (lfn q 16 36) 3)
                         (mce2 0.03 0.09)))))))

(define rzblp (add (f drand) (f dxrand)))

(hear rzblp)
