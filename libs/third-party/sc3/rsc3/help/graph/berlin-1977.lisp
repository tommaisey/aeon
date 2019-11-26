;; berlin 1977 (jmcc) #4

(define sequ (lambda (s tr) (demand tr 0 (dseq dinf (make-mce s)))))

(define sequ-r (lambda (s tr) (demand tr 0 (dshuf dinf (make-mce s)))))

(define berlin-1977
  (let* ((clock-rate (mouse-x kr 5 20 exponential 0.2))
         (clock-time (fdiv 1 clock-rate))
         (clock (impulse kr clock-rate 0))
         (pattern-list (list 55 60 63 62 60 67 63 58))
         (note (sequ pattern-list clock))
         (clock-16 (pulse-divider clock 16 0))
         (note* (add (sequ-r (list -12 -7 -5 0 2 5) clock-16) note))
         (freq (midi-cps note*))
         (env (decay2 clock (mul 0.05 clock-time) (mul 2 clock-time)))
         (amp (mul-add env 0.1 0.02))
         (filt (mul-add env (mul (f-sin-osc kr 0.17 0) 800) 1400))
         (pw (mul-add (sin-osc kr 0.08 (mce2 0 half-pi)) 0.45 0.5))
         (s (mul (pulse ar freq pw) amp)))
    (comb-n (rlpf s filt 0.15) 0.2 (mce2 0.2 0.17) 1.5)))

(hear berlin-1977)
