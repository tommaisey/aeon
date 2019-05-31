;; reverberated noise bursts (jmcc) #3

(define useq (lambda (n f) (foldl1 compose (replicate n f))))

(define rnb
  (let* ((s (mul (decay (mul (dust ar 0.6) 0.2) 0.15) (pink-noise ar)))
         (z (delay-n s 0.048 0.048))
         (y (mix (comb-l z 0.1 (mul-add (lf-noise1 kr (clone 6 (rand 0 0.1))) 0.04 0.05) 15)))
         (f (useq 4 (lambda (i) (allpass-n i 0.050 (clone 2 (rand 0 0.05)) 1)))))
    (add s (f y))))

(hear rnb)
