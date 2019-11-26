(hear
 (let* ((a (dser dinf (make-mce (list 1 3 2 7 8))))
        (x (mouse-x kr 1 40 1 0.1))
        (t (impulse kr x 0))
        (f (mul-add (demand t 0 a) 30 340)))
   (mul (sin-osc ar f 0) 0.1)))
