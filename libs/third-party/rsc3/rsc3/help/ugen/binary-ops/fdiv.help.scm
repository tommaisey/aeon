;; (fdiv a b)

;; Division, written '/' in sclang.

;; Division can be tricky with signals because of division by zero.

(audition
   (out 0 (fdiv (mul (pink-noise ar) 0.1) 
		(mul (f-sin-osc kr 10 0.5) 0.75))))
