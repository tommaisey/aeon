;; plucked strings (jmcc)

(define plucked-strings
  (let ((s (lambda (_)
             (let* ((s0 (mul (impulse ar (rand 2 2.2) 0) 0.3))
                    (s1 (mul (dust ar 0.5) 0.3))
                    (f (mul-add (sin-osc kr (rand 0.05 0.15) (rand 0 (* pi 2))) 5 5.2))
                    (s2 (mul (impulse ar f 0) 0.3))
                    (im (select (i-rand 0 2) (mce3 s0 s1 s2)))
                    (dt (fdiv 1 (midi-cps (u:floor (rand 60 90)))))
                    (t (mul3 (decay im 0.1) (pink-noise ar) 0.1)))
               (pan2 (comb-l t dt dt 4) (rand -1 1) 1)))))
    (leak-dc (mix-fill 5 s) 0.96)))

(hear plucked-strings)
