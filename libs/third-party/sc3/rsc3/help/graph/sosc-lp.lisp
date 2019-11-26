;; sosc-lp (rd)

;; (hear (dust-r* ar 0.1 1))
(define dust-r*
  (lambda (r lo hi)
    (let ((d (dseq dinf (make-mce (list (dwhite 1 lo hi))))))
      (t-duty r d 0 0 (u:abs (white-noise r)) 1))))

;; (b0 (as-local-buf (list 60 71 89 65 36 57 92 97 92 97)))
;; (b1 (as-local-buf (list 71 89 60 57 65 36 95 92 93 97)))

;; controls image synthesiser, ie. send-trig...
(define sosc-lp
  (let* ((clk (dust-r* kr 0.2 0.9))
         (env (decay2 clk 0.002 2.5))
         (idx (stepper clk 0 0 15 1 0))
         (f1 (midi-cps (mce2 (sub (buf-rd 1 kr 10 idx 1 1) 24)
                             (sub (buf-rd 1 kr 11 idx 1 1) 24))))
         (f2 (add f1 (mul (lf-noise0 kr (mce2 1 3)) 1.2)))
         (o1 (mul (sin-osc ar f1 0) env))
         (o2 (mul (sin-osc ar f2 0) env)))
    (mrg2
     (mul (add o1 o2) 0.2)
     (send-trig clk 0 clk))))

(with-sc3
 (lambda (fd)
   (let ((a (list 60 71 89 65 36 57 92 97 92 97))
         (b (list 71 89 60 57 65 36 95 92 93 97)))
     (begin
       (async fd (b-alloc 10 9 1))
       (async fd (b-alloc 11 9 1))
       (send fd (b-setn1 10 0 a))
       (send fd (b-setn1 11 0 b))
       (play fd (out 0 sosc-lp))))))

;; (alternate nil)
(define alternate
  (lambda (_)
    (with-sc3
     (lambda (fd)
       (let ((a (list 71 60 65 89 36 57 95 97 92 97))
             (b (list 89 71 60 65 57 36 92 95 93 97)))
         (begin
           (send fd (b-setn1 10 0 a))
           (send fd (b-setn1 11 0 b))))))))
