;; (pv-conformal-map buffer real imag)

;; Applies the conformal mapping z send (z-a)/(1-za*) to the phase
;; vocoder bins z with a given by the real and imag imputs to the
;; UGen.

;; See http://mathworld.wolfram.com/ConformalMapping.html

;; buffer - buffer number of buffer to act on, passed in through a chain
;; real   - real part of a.
;; imag   - imaginary part of a.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 1024 1))
   (async fd (b-alloc 0 2048 1))))

(audition
 (out 0 (pan2
	 (ifft*
	  (pv-conformal-map
	   (fft* 10 (mul (sound-in 0) 0.5)) (mouse-x kr -1 1 0 0.1)
	   (mouse-y kr -1 1 0 0.1)))
	 0
	 1)))

(let* ((signal (lambda (n)
		 (let* ((o (sin-osc kr (mix-fill n (lambda (_) (rand 0.1 0.5))) 0))
			(a (mul (make-mce (list 1 1.1 1.5 1.78 2.45 6.7)) 220))
			(f (mul-add o 10 a)))
		   (mix (mul (lf-saw ar f 0) 0.3)))))
       (mapped (lambda (n)
		 (let* ((c0 (fft* 0 (signal n)))
			(x (mouse-x kr 0.01 2.0  1.0 0.1))
			(y (mouse-y kr 0.01 10.0 1.0 0.1))
			(c1 (pv-conformal-map c0 x y)))
		   (ifft* c1))))
       (s (mapped 3))
       (t (mul-add (comb-n s 0.1 0.1 10) 0.5 s)))
  (audition (out 0 (pan2 t 0 1))))
