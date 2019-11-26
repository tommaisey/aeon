(hear
 (let* ((x (mouse-x kr 0 4 0 0.1))
        (y (mouse-y kr 1 15 0 0.1))
        (a (dswitch1 x (make-mce (list 1 3 y 2 (dwhite 2 0 3)))))
        (t (impulse kr 3 0))
        (f (mul-add (demand t 0 a) 30 340)))
   (mul (sin-osc ar f 0) 0.1)))
