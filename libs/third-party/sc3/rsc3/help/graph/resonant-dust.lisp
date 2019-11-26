;; resonant dust (jmcc) #2

(define resonant-dust
  (let* ((rf (let* ((st (rand 80 2080))
                    (en (add st (mul (rand -0.5 0.5) st))))
               (x-line kr st en 9 do-nothing)))
         (d (mul (dust ar (rand 50 850)) 0.3)))
    (pan2 (resonz d rf 0.1) (rand -1 1) 1)))

(with-sc3 (overlap-texture-u (list 5 2 9 +inf.0) resonant-dust))
