;; strummable guitar (jmcc) #11

(define strummable-guitar
  (let* ((scale (list 52 57 62 67 71 76))
         (str (lambda (i)
                (let* ((x (mouse-x kr 0 1 0 0.2))
                       (t (u:abs (hpz1 (u:gt x (add 0.25 (mul i 0.1))))))
                       (e (decay t 0.05))
                       (n (mul (pink-noise ar) e))
                       (dt (recip (midi-cps (list-ref scale i))))
                       (s (comb-l n dt dt 4)))
                  (pan2 s (sub (mul i 0.2) 0.5) 1))))
         (strs (mix-fill (length scale) str)))
    (leak-dc (lpf strs 12000) 0.995)))

(hear strummable-guitar)
