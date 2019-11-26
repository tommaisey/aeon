;; tank (jmcc)

(define bang
  (let ((d (dust ar 0.01))
        (n (brown-noise ar)))
    (pan2 (mul (decay2 d 0.04 0.3) n) 0 1)))

(define pling
  (lambda (_)
    (let ((o (f-sin-osc ar (exp-rand 300 2200) 0))
          (d (decay2 (dust ar 0.5) 0.1 0.5)))
      (pan2 (mul (mul d 0.1) (cubed o)) (rand -1 1) 1))))

(define rallpass
  (lambda (i)
    (allpass-n i 0.03 (clone 2 (rand 0.005 0.02)) 1)))

(define rallpasses
  (foldl1 compose (replicate 4 rallpass)))

(define tank
  (lambda (i)
    (let* ((l0 (mul (local-in 2 ar (mce2 0 0)) 0.98))
           (l1 (one-pole l0 0.33))
           (l2 (rotate2 (mce-channel l1 0) (mce-channel l1 1) 0.23))
           (l3 (allpass-n l2 0.05 (clone 2 (rand 0.01 0.05)) 2))
           (l4 (delay-n l3 0.3 (mce2 0.17 0.23)))
           (l5 (allpass-n l4 0.05 (clone 2 (rand 0.03 0.15)) 2))
           (l6 (leak-dc l5 0.995))
           (l7 (add l6 i)))
      (mrg2 l7 (local-out l7)))))

(hear (tank (rallpasses (add bang (mix-fill 8 pling)))))
