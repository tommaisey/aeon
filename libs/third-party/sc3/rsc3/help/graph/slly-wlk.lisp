;; slly-walk (rd)

(define o1
  (let* ((t (dust kr 2))
         (e (env-bp-linear (list 0 0 (t-rand 0.05 0.5 t) 1 1 0) 1 1))
         (d (t-rand 0.15 0.25 t))
         (a (env-gen kr t (t-rand 0.02 0.04 t) 0 d do-nothing e))
         (p (env-gen kr t (t-rand 1.0 2.0 t) -1.0 d do-nothing e))
         (n (t-choose t (mce4 0 2 5 7)))
         (o 72))
    (pan2 (sin-osc ar (midi-cps (add n o)) 0) p a)))

(define shft
  (lambda ()
    (let* ((t (impulse kr 12 0))
           (e (env-bp-linear (list 0 0 (t-rand 0.25 1.0 t) 1 1 0) 1 1))
           (d (t-rand 0.05 0.125 t))
           (a (env-gen kr t (t-rand 0.02 0.04 t) 0 d do-nothing e))
           (p (env-gen kr t (t-rand 1.0 2.0 t) -1.0 d do-nothing e))
           (n (t-choose t (make-mce (list 0 0 5 7 7 12 12 19))))
           (o (add 84 (mul (toggle-ff t) 12))))
      (pan2 (saw ar (midi-cps (add n o))) p a))))

(define o2
  (mul (add3 (shft) (shft) (shft)) 0.1))

(define o3
  (let* ((t (impulse kr 6 0))
         (e (env-bp-linear (list 0 0 (t-rand 0.25 1.0 t) 1 1 0) 1 1))
         (d (t-rand 0.05 0.25 t))
         (a (env-gen kr t (t-rand 0.01 0.02 t) 0 d do-nothing e))
         (p (env-gen kr t (t-rand 1.0 2.0 t) -1.0 d do-nothing e))
         (n (demand t 0 (dibrown dinf 0 7 1)))
         (o 52))
    (pan2 (saw ar (midi-cps (degree-to-key 0 (add n o) 12))) p a)))

(define o4
  (mul (rlpf (lf-pulse ar (midi-cps (mce2 36 43)) 0.15 0.5)
             (midi-cps (mul-add (sin-osc kr 0.1 0) 10 36))
             0.1)
       0.1))

(define slly-wlk
  (lambda (fd)
    (begin
      (async fd (b-alloc 0 7 1))
      (send fd (b-setn1 0 0 (list 0 2 3.2 5 7 9 10)))
      (play fd (out 0 (add4 o1 o2 o3 o4))))))

(with-sc3 slly-wlk)
