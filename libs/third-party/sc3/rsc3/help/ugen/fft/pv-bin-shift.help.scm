;; (pv-bin-shift buffer stretch shift)

;; Shift and scale the positions of the bins.  Can be used as a very
;; crude frequency shifter/scaler.  Shifts the leftmost bin at
;; `buffer' by `shift' places, the distance between subsequent bins is
;; `stretch'.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))))

(define snd
  (let* ((f1 (squared (mul-add (sin-osc kr 0.08 0) 6 6.2)))
	 (f2 (sin-osc kr f1 0)))
    (sin-osc ar (mul-add f2 100 800) 0)))

(audition (out 0 snd))

(audition
 (out 0 (mul
	 (ifft*
	  (pv-bin-shift
	   (fft* 10 snd)
	   (mouse-y kr 1 4 0 0.1)
	   (mouse-x kr -10 100 0 0.1)))
	 1/2)))
