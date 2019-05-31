;; chrd (rd)

(define chrd
  (lambda (_)
    (let* ((m (make-mce (list 60 65 72 77 79 84)))
           (ds 3)
           (d (make-mce (map (lambda (x) (* x ds)) (list 5 4 5 7 4 5))))
           (l (x-line kr m (add m (rand 0.05 0.5)) d do-nothing))
           (f (midi-cps l))
           (z (env-trapezoid 0 (rand 0.15 0.35) d (rand 0.005 0.01)))
           (e (env-gen kr 1 1 0 1 do-nothing z))
           (p (x-line kr (rand -1 1) (rand -1 1) d do-nothing))
           (o (f-sin-osc ar f 0)))
      (mix (pan2 o p e)))))

(define chrdn
  (lambda (n)
    (mix-fill n chrd)))

(hear (chrdn 5))
