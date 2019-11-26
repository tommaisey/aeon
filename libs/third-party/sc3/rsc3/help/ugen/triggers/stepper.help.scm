(import (rhs) (rsc3))

(with-sc3
 (lambda (fd)
   (let ((a (list 97.999 195.998 523.251 466.164 195.998
		  233.082 87.307 391.995 87.307 261.626
		  195.998 77.782 233.082 195.998 97.999
		  155.563)))
     (async fd (b-alloc 10 128 1))
     (send fd (b-setn1 10 0 a)))))

;; Function composition...

(define (seq s l)
  (if (null? l)
      s
      (seq ((car l) s) (cdr l))))

(let* ((rate (mouse-x kr 1 5 1 0.1))
       (clock (impulse kr rate 0))
       (env (decay2 clock 0.002 2.5))
       (index (stepper clock 0 0 15 1 0))
       (freq (buf-rd 1 kr 10 index 1 1))
       (ffreq (add (if #t
		       (lag2 freq 0.1)
		       (mul (mouse-y kr 80 1600 1 0.1) (add (Mul env 4) 2)))
		   (mce2 0 0.3)))
       (lfo (sin-osc kr 0.2 (mce4 0 (/ pi 2) 0.0024 0.0025)))
       (rvb (lambda (s) (allpass-n s
				  0.05
				  (clone 2 (rand 0 0.05))
				  (rand 1.5 2.0))))
       (proc (list
	      (lambda (s) (mul (rlpf s ffreq 0.3) env))
	      (lambda (s) (mul (rlpf s ffreq 0.3) env))
	      (lambda (s) (mul s 0.2))
	      ;; Echo
	      (lambda (s) (mul-add (comb-l s 1 (fdiv 0.66 rate) 2) 0.8 s))
	      ;; Reverb
	      (lambda (s) (add s (mul (seq s (replicate 5 rvb)) 0.3)))
	      (lambda (s) (leak-dc s 0.1))
	      ;; Flanger
	      (lambda (s) (add (delay-l s 0.1 lfo) s))
	      ;; Slight bass emphasis
	      (lambda (s) (one-pole s 0.9))))
       (init (mix (lf-pulse ar (mul freq (mce3 1 3/2 2)) 0 0.3))))
  (audition (out 0 (seq init proc))))

;; Pattern randomizer....

(with-sc3
 (lambda (fd)
   (let ((p (map (lambda (e)
		   (midi-cps (+ 36 (s:degree-to-key e (list 0 3 5 7 10) 12))))
		 (map floor (replicate-m 16 (random 0 15))))))
     (send fd (b-setn1 10 0 p)))))

;; A shorter variant, using some simple syntax...

(define-syntax seq*
  (syntax-rules ()
    ((_ i s f ...)
     (seq i (list (lambda (s) f) ...)))))

(let* ((rate (mouse-x kr 1 5 1 0.1))
       (clock (impulse kr rate 0))
       (env (decay2 clock 0.002 2.5))
       (index (stepper clock 0 0 15 1 0))
       (freq (buf-rd 1 kr 10 index 1 1))
       (ffreq (add (lag2 freq 0.1) (mce2 0 0.3)))
       (lfo (sin-osc kr 0.2 (mce4 0 (/ pi 2) 0.0024 0.0025)))
       (rvb (lambda (s) (allpass-n s
				  0.05
				  (clone 2 (rand 0 0.05))
				  (rand 1.5 2.0))))
       (init (mix (lf-pulse ar (mul freq (mce3 1 3/2 2)) 0 0.3)))
       (proc (seq* init
		   s
		   (mul (rlpf s ffreq 0.3) env)
		   (mul (rlpf s ffreq 0.3) env)
		   (mul s 0.2)
		   (mul-add (comb-l s 1 (fdiv 0.66 rate) 2) 0.8 s)
		   (add s (mul (seq s (replicate 5 rvb)) 0.3))
		   (leak-dc s 0.1)
		   (add (delay-l s 0.1 lfo) s)
		   (one-pole s 0.9))))
  (audition (out 0 proc)))
