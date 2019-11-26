(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 24 1))
   (send fd (b-setn1 10 0 (replicate-m 24 (exp-random 200 500))))))

(let* ((q (dseq 3 (make-mce (list 0 3 5 0 3 7 0 5 9))))
       (p (dseq dinf (mce2 q (dbrown 5 0 23 1))))
       (t (dust kr 10)))
  (audition (out 0 (mul (sin-osc ar (demand t 0 (dbufrd 10 p 1)) 0) 0.1))))

;; Buffer as a time pattern.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 11 24 1))
   (send fd (b-setn1 11 0 (replicate-m 24 (choose (list 1 0.5 0.25)))))))

(let* ((p (dseq dinf (mce2 (dseq 3 (make-mce (list 0 3 5 0 3 7 0 5 9)))
                           (dbrown 5 0 23 1))))
       (d (mul (dbufrd 11 (dseries dinf 0 1) 1) 0.5))
       (l (dbufrd 10 p 1)))
  (audition (out 0 (mul (sin-osc ar (duty kr d 0 do-nothing l) 0) 0.1))))

;; free buffers

(with-sc3
 (lambda (fd)
   (async fd (b-free 10))
   (async fd (b-free 11))))
