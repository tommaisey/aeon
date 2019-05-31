(hear
 (let* ((dur (drand dinf (mce3 0.01 0.2 0.4)))
        (amp (dseq dinf (make-mce (list 204 400 201 502 300 200))))
        (f (duty kr dur 0 do-nothing amp))
        (o (sin-osc ar (mul f (mce2 1 1.01)) 0)))
   (mul o 0.1)))

(hear
 (let* ((dur (mouse-x kr 0.001 2 1 0.1))
        (amp (dseq dinf (make-mce (list 204 400 201 502 300 200))))
        (f (duty kr dur 0 do-nothing amp))
        (o (sin-osc ar (mul f (mce2 1 1.0)) 0)))
   (mul o 0.1)))
