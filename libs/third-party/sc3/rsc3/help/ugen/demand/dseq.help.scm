(hear
 (let* ((a (dseq 3 (make-mce (list 1 3 2 7 8))))
        (t (impulse kr (mouse-x kr 1 40 1 0.1) 0))
        (f (mul-add (demand t 0 a) 30 340)))
   (mul (sin-osc ar f 0) 0.1)))

(hear
 (let* ((a (dseq dinf (clone 32 (rand 0 10))))
        (t (impulse ar (mouse-x kr 1 10000 1 0.1) 0))
        (f (mul-add (demand t 0 a) 30 340)))
   (mul (sin-osc ar f 0) 0.1)))
