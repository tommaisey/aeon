(hear
 (let* ((a0 (dwhite 2 3 4))
        (a1 (dwhite 2 0 1))
        (a2 (dseq 2 (make-mce (list 1 1 1 0))))
        (i (dseq 2 (make-mce (list 0 1 2 1 0))))
        (d (dswitch i (make-mce (list a0 a1 a2))))
        (t (impulse kr 4 0))
        (f (mul-add (demand t 0 d) 300 400)))
   (mul (sin-osc ar f 0) 0.1)))

;; compare with dswitch1

(hear
 (let* ((a0 (dwhite 2 3 4))
        (a1 (dwhite 2 0 1))
        (a2 (dseq 2 (make-mce (list 1 1 1 0))))
        (i (dseq 2 (make-mce (list 0 1 2 1 0))))
        (d (dswitch1 i (make-mce (list a0 a1 a2))))
        (t (impulse kr 4 0))
        (f (mul-add (demand t 0 d) 300 400)))
   (mul (sin-osc ar f 0) 0.1)))
