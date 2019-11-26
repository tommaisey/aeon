;; (coin-gate prob in)

;; When it receives a trigger, it tosses a coin, and either passes the
;; trigger or doesn't.

(let ((f (t-rand 300 400 (Coingate 0.8 (impulse kr 10 0)))))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))

(let* ((p 0.2)
       (t (mul (impulse ar 20 0) (add (sin-osc kr 0.5 0) 1)))
       (t* (t-exp-rand (Mce 1000 1000) 12000 t))
       (i (lambda () (Coingate (+ p (rand 0 0.1)) (mul t 0.5))))
       (s (lambda () (ringz (i) t* 0.01)))
       (ignore (lambda (f) (lambda (_) (f)))))
  (audition (out 0 (mix/fill 3 (ignore s)))))
