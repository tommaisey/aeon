;; trmlo (rd)

(define m-wrp
  (lambda (i l r)
    (lin-lin i -1 1 (midi-cps l) (midi-cps r))))

(define m-wrp1
  (lambda (i m)
    (m-wrp i m (add m 1))))

(define m-wrp-n
  (lambda (i m n)
    (m-wrp i m (add m n))))

(define o1
  (let* ((f 5)
         (d 3)
         (s (env-sine d 0.1))
         (e (env-gen kr 1 1 0 1 do-nothing s))
         (n 65)
         (m (sin-osc kr f 0)))
    (pan2 (sin-osc ar (m-wrp1 m n) 0) m e)))

(define o2
  (let* ((f (i-rand 5 9))
         (d (i-rand 5 9))
         (s (env-sine d (rand 0.1 0.2)))
         (e (env-gen kr 1 1 0 1 do-nothing s))
         (n (i-rand 69 72))
         (m (sin-osc kr f 0)))
    (pan2 (sin-osc ar (m-wrp1 m n) 0) m e)))

(define o3
  (let* ((f (i-rand 5 9))
         (d (i-rand 9 12))
         (s (env-sine d (rand 0.1 0.2)))
         (e (env-gen kr 1 1 0 1 do-nothing s))
         (n (i-rand 69 72))
         (m (sin-osc kr f 0))
         (l (line kr 0 (i-rand 1 5) d do-nothing)))
    (pan2 (blip ar (m-wrp1 m (add n l)) (lin-lin m -1 1 1 2)) m e)))

(define o4
  (let* ((f (i-rand 5 18))
         (d (i-rand 12 15))
         (s (env-sine d (rand 0.1 0.2)))
         (e (env-gen kr 1 0.05 0 1 do-nothing s))
         (n (i-rand 69 72))
         (m (sin-osc kr f 0))
         (l (line kr 0 (i-rand 1 5) d remove-synth))
         (fr (m-wrp-n m (add n l) (i-rand 1 5))))
    (pan2 (blip ar fr (lin-lin m -1 1 1 (i-rand 2 24))) m e)))

(hear (add4 o1 o2 o3 o4))
