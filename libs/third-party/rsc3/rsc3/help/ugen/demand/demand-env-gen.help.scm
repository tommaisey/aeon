;; Frequency envelope with random times.

(hear
 (let* ((l (dseq dinf (make-mce (list 204 400 201 502 300 200))))
        (t (drand dinf (make-mce (list 1.01 0.2 0.1 2.0))))
        (y (mouse-y kr 0.01 3 1 0.1))
        (f (demand-env-gen ar l (mul t y) 7 0 1 1 1 0 1 do-nothing)))
   (mul (sin-osc ar (mul f (mce2 1 1.01)) 0) 0.1)))

;; Frequency modulation

(hear
 (let* ((x (mouse-x kr -0.01 -4 0 0.1))
        (y (mouse-y kr 1 3000 1 0.1))
        (l (lambda () (dseq dinf (clone 32 (exp-rand 200 1000)))))
        (t (mul sample-dur y))
        (f (demand-env-gen ar (mce2 (l) (l)) t 5 x 1 1 1 0 1 do-nothing)))
   (mul (sin-osc ar f 0) 0.1)))

;; Gate, mouse-x on right side of screen toggles gate.

(hear
 (let* ((x (mouse-x kr 0 1 0 0.1))
        (l (u:round (dwhite dinf 300 1000) 100))
        (f (demand-env-gen kr l 0.1 5 0.3 (gt x 0.5) 1 1 0 1 do-nothing)))
   (mul (sin-osc ar (mul f (mce2 1 1.21)) 0) 0.1)))
