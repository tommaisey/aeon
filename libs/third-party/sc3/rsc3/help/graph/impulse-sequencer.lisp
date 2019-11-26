;; impulse sequencer (jmcc) SC2

(define isequ (lambda (s tr) (mul tr (demand tr 0 (dseq dinf (make-mce s))))))

(define impulse-sequencer
  (let* ((t (impulse ar 8 0))
	 (c-sq (isequ (list 1 0 0 1 0 0 1 0 0 0 1 0 1 0 0 0) t))
	 (c (mul3 (decay2 c-sq 0.001 0.3) (f-sin-osc ar 1700 0) 0.1))
	 (d-sq (isequ (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0) t))
	 (d (mul3 (decay2 d-sq 0.001 0.3) (f-sin-osc ar 2400 0) 0.04))
	 (n-sq (isequ (list 1.0 0.1 0.1 1.0 0.1 1.0 0.1 0.1) t))
	 (n (mul3 (decay2 n-sq 0.001 0.25) (brown-noise ar) 0.1))
	 (b-sq (isequ (list 1 0 0.2 0 0.2 0 0.2 0) t))
	 (b (mul3 (decay2 b-sq 0.001 0.5) (f-sin-osc ar 100 0) 0.2)))
    (add4 c d n b)))

(hear impulse-sequencer)
