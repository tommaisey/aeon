;; tr-out (rd)

(define tr-out
  (let* ((node
          (lambda (n)
            (let* ((t (dust kr 1.6))
                   (f (midi-cps (buf-rd-n 1 kr 0 (t-rand 0 6 t) 0)))
                   (p (buf-rd-n 1 kr 1 (t-rand 0 6 t) 0))
                   (a (buf-rd-n 1 kr 2 (t-rand 0 6 t) 0)))
              (cons (pan2 (sin-osc ar f 0) p a)
                    (send-trig t n (fdiv f 660))))))
         (ns (map node (enum-from-to 1 4))))
    (mrg-n (cons (foldl1 add (map car ns))
                 (map cdr ns)))))

(with-sc3
 (lambda (fd)
   (begin
     (async fd (b-alloc 0 6 1))
     (send fd (b-setn1 0 0 (list 60 62 64 65 67 69)))
     (async fd (b-alloc 1 6 1))
     (send fd (b-setn1 1 0 (list -1 -0.5 0 0.25 0.75 1.0)))
     (async fd (b-alloc 2 6 1))
     (send fd (b-setn1 2 0 (list 0.01 0.05 0.1 0.15 0.25 0.35)))
     (play fd (out 0 tr-out)))))
