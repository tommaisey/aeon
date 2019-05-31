; why supercollider (jmcc) #0

(define why-supercollider?
  (let* ((r (resonz (mul (dust ar 0.2) 50) (rand 200 3200) 0.003))
	 (s (mix (clone 10 r)))
	 (z (delay-n s 0.048 0.048))
	 (c (comb-l z 0.1 (mul-add (lf-noise1 kr (rand 0 0.1)) 0.04 0.05) 15))
	 (y (mix (clone 7 c)))
	 (f (lambda (i) (allpass-n i 0.05 (clone 2 (rand 0 0.05)) 1)))
	 (x ((foldl1 compose (replicate 4 f)) y)))
    (add s (mul x 0.2))))

(hear why-supercollider?)
