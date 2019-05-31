;; reverberated sine percussion (jmcc) #3

(define rallpass
  (lambda (i)
    (allpass-n i 0.050 (clone 3 (rand 0 0.05)) 1)))

(define chain-of
  (lambda (n f)
    (foldl1 compose (replicate n f))))

(define reverberated-sine-percussion
  (let* ((d 10)
         (c 7)
         (a 4)
         (s (mix-fill
             d
             (lambda (_)
               (resonz (mul (dust ar (/ 2 d)) 50)
                       (rand 200 3200)
                       0.003))))
         (z (delay-n s 0.048 0.48))
         (y (mix (comb-l z
                         0.1
                         (mul-add (lf-noise1 kr (clone c (rand 0 0.1)))
                                  0.04
                                  0.05)
                         15)))
         (x ((chain-of a rallpass) y)))
    (add s (mul 0.2 x))))

(hear reverberated-sine-percussion)
