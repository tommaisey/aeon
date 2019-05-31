(hear
 (let ((f (lambda (u)
            (let* ((a (u dinf (make-mce (list 1 3 2 7 8))))
                   (t (impulse kr (mouse-x kr 1 400 1 0.1) 0))
                   (f (mul-add (demand t 0 a) 30 340)))
              (mul (sin-osc ar f 0) 0.1)))))
   (mce2 (f drand) (f dxrand))))
