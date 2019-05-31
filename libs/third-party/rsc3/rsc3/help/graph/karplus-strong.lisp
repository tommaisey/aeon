;; karplus strong (alex mclean)

(define a-a
  (list "a-a"
        (list 800 1150 2800 3500 4950)
        (list 0 -4   -20 -36  -60)
        (list 80 90  120 130 140)))

(define a-u
  (list "a-u"
        (list 325 700 2530 3500 4950)
        (list 0 -12  -30 -40 -64)
        (list 50 60  170 180 200)))

(define cs
  (lambda (l)
    (concat
     (list (list-ref l 1)
           (list-ref l 2)
           (list-ref l 3)))))

(define vf
  (lambda (i s)
    (let ((f (in 5 kr i))
          (a (in 5 kr (add i 5)))
          (b (in 5 kr (add i 10))))
      (mix (mul (resonz s f (fdiv b f)) (db-amp a))))))

(define ugen-if
  (lambda (a b c)
    (add (mul a b) (mul (sub 1 a) c))))

(define prob-switch
  (lambda (n0 i prob)
    (ugen-if (u:gt n0 prob) i (neg i))))

(define ks
  (lambda (n d)
    (let* ((x (mouse-x kr 0 0.01 linear 0.1)) ;; {- delay -}
           (y (mouse-y kr 0.85 1 linear 0.1)) ;; {- blend / gain -}
           (n0 (add (fdiv n 2) 0.5))
           (lagged-delay (lag x 0.01))
           (o (sin-osc ar 200 0))
           (a0 (mul (decay d 0.025) o))
           (a1 (add (local-in 1 ar 0) (mul a0 (sub y 0.25))))
           (a2 (delay-n a1 0.01 lagged-delay))
           (a3 (delay1 a2))
           (a4 (fdiv (add a2 a3) 2.0))
           (a5 (prob-switch n0 a4 y))
           (a6 (vf (mul (toggle-ff d) 15) a5))
           (a7 (mul a6 1.5)))
      (mrg2 (local-out (mul a5 0.99))
            (out 0 (mce2 a7 a7))))))

(define karplus-strong
  (lambda (fd)
    (begin
      (send fd (c-setn1 0 (cs a-a)))
      (send fd (c-setn1 15 (cs a-u)))
      (let ((n (white-noise ar))
            (d (dust kr 4)))
        (play fd (ks n d))))))

(with-sc3 karplus-strong)
