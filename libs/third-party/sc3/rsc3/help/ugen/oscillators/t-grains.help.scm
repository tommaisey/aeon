(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 10 "/home/rohan/audio/metal.wav" 0 0))))

(audition
 (let* ((b 10)
	(trate (mouse-y kr 2 200 1 0.1))
	(dur (fdiv 4 trate))
	(t (impulse ar trate 0))
	(i (mouse-x kr 0 (buf-dur kr b) 0 0.1)))
  (out 0 (t-grains 2 t b 1 i dur 0 0.1 2))))

(audition
 (let* ((b 10)
	(trate (mouse-y kr 8 120 1 0.1))
	(dur (fdiv 12 trate))
	(clk (impulse ar trate 0))
	(x (mouse-x kr 0 (buf-dur kr b) 0 0.1))
	(pos (add x (t-rand 0 0.01 clk)))
	(pan (mul (white-noise kr) 0.6)))
   (out 0 (t-grains 2 clk b 1 pos dur pan 0.1 2))))

(audition
 (let* ((b 10)
	(trate (mouse-y kr 8 120 1 0.1))
	(dur (fdiv 4 trate))
	(clk (dust ar trate))
	(x (mouse-x kr 0 (buf-dur kr b) 0 0.1))
	(pos (add x (t-rand 0 0.01 clk)))
	(pan (mul (white-noise kr) 0.6)))
   (out 0 (t-grains 2 clk b 1 pos dur pan 0.1 2))))

;; The SC3 ** operator is the ShiftLeft binary UGen.

(audition
 (let* ((b 10)
	(trate (mouse-y kr 2 120 1 0.1))
	(dur (fdiv 1.2 trate))
	(clk (impulse ar trate 0))
	(pos (mouse-x kr 0 (buf-dur kr b) 0 0.1))
	(pan (mul (white-noise kr) 0.6))
	(rate (shift-left 1.2 (u:round (mul (white-noise kr) 3) 1))))
   (out 0 (t-grains 2 clk b rate pos dur pan 0.1 2))))
