;; analogue daze (jmcc) #3

(define analogue-daze
  (let* ((pattern (list 55 63 60 63 57 65 62 65))
	 (sequ (lambda (s tr) (demand tr 0 (dseq dinf (make-mce s)))))
	 (f (lambda (octave clockrate pwm-rate flt-rate)
	      (let* ((trg (impulse kr clockrate 0))
		     (pattern* (map (compose midi-cps
					     (lambda (x) (add x (mul 12 octave))))
				    pattern))
		     (sq (sequ pattern* trg))
		     (pwm (mul-add (sin-osc kr pwm-rate (rand 0 two-pi)) 0.4 0.5))
		     (cf (mul-add (sin-osc kr flt-rate (rand 0 two-pi)) 1400 2000)))
		(rlpf (mul (lf-pulse ar (lag sq 0.05) 0 pwm) 0.1) cf (fdiv 1 15)))))
	 (a (mul (lf-noise0 ar (mul-add (lf-noise1 kr 0.3) 6000 8000)) (mce2 0.07 0.08)))
	 (x (mul (decay (impulse ar 2 0) 0.15) a))
	 (g (add (mce2 (f 1 8 0.31 0.2) (f 0 2 0.13 0.11)) x))
	 (z (mul 0.4 (add (comb-n g 0.375 0.375 5) (mce-reverse g))))
	 (e (env-linen 2 56 2 1 nil)))
    (mul z (env-gen kr 1 1 0 1 remove-synth e))))

(hear analogue-daze)
