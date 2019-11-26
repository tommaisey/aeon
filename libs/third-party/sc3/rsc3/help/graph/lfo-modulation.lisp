;; lfo modulation (jmcc) #1

(define lfo-modulation
  (let* ((o (mul-add (f-sin-osc kr 0.05 0) 80 160))
         (p (mul-add (f-sin-osc kr (mce2 0.6 0.7) 0) 3600 4000))
         (s (rlpf (mul (lf-pulse ar o 0 0.4) 0.05) p 0.2)))
    (comb-l s 0.3 (mce2 0.2 0.25) 2)))

(hear lfo-modulation)
