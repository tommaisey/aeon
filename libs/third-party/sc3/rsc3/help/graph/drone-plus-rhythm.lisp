(define scale (list 0 2 3 5 7 9 10))

(define rand2 (lambda (n) (rand (negate n) n)))

(define drone-1
  (let* ((f0 (midi-cps (add (l-choose (list 24 36)) (rand 0 8.0e-2))))
	 (f1 (mul (lf-saw ar (mce2 f0 (add f0 0.2)) 0)
		  (lf-noise2 kr (mul f0 (mce2 0.05 0.04))))))
    (lpf (mul 0.06 f1) (rand 1000 3000))))

(define drone-1-txt (overlap-texture-u (list 4 4 8 dinf) drone-1))

(define drone-2
  (let ((x (u:gt (rand 0 1) 0.8))
        (m (add (add (l-choose (list 60 72)) (l-choose scale)) (clone 2 (rand2 0.05)))))
    (mul (mul (sin-osc ar (midi-cps m) 0) x) (rand 0.04 0.07))))

(define drone-2-txt (overlap-texture-u (list 4 6 3 dinf) drone-2))

(define iseqr (lambda (s tr) (mul tr (demand tr 0 (dxrand dinf (make-mce s))))))

(define rhy
  (let* ((m (add (add (l-choose (list 48 60 72 84)) (l-choose scale)) (clone 2 (rand2 0.03))))
	 (sq (iseqr (list 0 1 0 1 1 0) (impulse ar (l-choose (list 1.5 3 6)) 0)))
	 (sg (mul (lf-pulse ar (midi-cps m) 0 0.4) (rand 0.03 0.08))))
    (rlpf (mul (decay2 sq 0.004 (rand 0.2 0.7)) sg) (exp-rand 800 2000) 0.1)))

(define pp (lambda (z) (add (comb-n z 0.5 0.5 6) (mce-reverse z))))

(define rhy-txt (overlap-texture-u (list 6 6 6 dinf) rhy))

(begin
  (fork (with-sc3 drone-1-txt))
  (fork (with-sc3 drone-2-txt))
  (fork (with-sc3* (list (post-process-u 2 pp) rhy-txt))))
