(define mirror1 (lambda (l) (append l (cdr (reverse (cdr l))))))

(hear
 (let* ((t (impulse kr 24 0))
        (s (drand dinf (mce2 (dseq 1 (make-mce (mirror1 (enum-from-to 1 5))))
                             (drand 8 (make-mce (enum-from-to 4 11))))))
        (f (demand t 0 (mul s 100)))
        (x (mouse-x kr -1 1 0 0.1))
        (o (sin-osc ar (mce2 f (add f 0.7)) 0)))
   (mul (scale-neg (cubed (cubed o)) x) 0.1)))

(hear
 (let* ((t (impulse kr 10 0))
        (r (dust kr 1))
        (s (dgeom dinf (midi-cps 72) (midi-ratio 1)))
        (f (demand t r s))
        (o (sin-osc ar (mce2 f (add f 0.7)) 0)))
   (mul (u:max (cubed o) 0) 0.1)))

(hear
 (let* ((t (impulse kr 10 0))
        (s (midi-cps (diwhite dinf 60 72)))
        (f (demand t 0 s))
        (o (sin-osc ar (mce2 f (add f 0.7)) 0)))
   (mul (cubed (cubed o)) 0.1)))
