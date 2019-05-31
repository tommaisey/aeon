;; rails (jmcc) #2

(define rails
  (let* ((n 20) ; resonant modes
         (e (mul (dust ar 100) 0.04)) ; excitation
         (f (x-line kr 3000 300 8 do-nothing)) ; sweep filter down
         (l (line kr (rand2 1) (rand2 1) 8 do-nothing)) ; sweep pan
         (r (clone n (add 200 (lin-rand 0 3000 0)))) ; resonant frequencies
         (a (make-mce (replicate n 1)))
         (t (clone n (add 0.2 (rand 0 1)))) ; ring times
         (k (klank (resonz e f 0.2) 1 0 1 (klank-data-mce r a t))))
    (pan2 k l 1)))

(with-sc3 (overlap-texture-u (list 3 2 4 +inf.0) rails))
