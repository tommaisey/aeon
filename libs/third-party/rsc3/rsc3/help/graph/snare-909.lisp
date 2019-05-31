;; snare-909 (jmcc)

(define snr
  (lambda (tr n v)
    (let* ((mk-e
            (lambda (a b)
              (let ((p (env-perc a b 1 (list -4 -4))))
                (env-gen ar tr 1 0 1 do-nothing p))))
           (e1 (mk-e 0.0005 0.055))
           (e2 (mk-e 0.0005 0.075))
           (e3 (mk-e 0.0005 0.4))
           (e4 (mk-e 0.0005 0.283))
           (t1 (lf-tri ar 330 0))
           (t2 (lf-tri ar 185 0))
           (x1 (mul-add (lpf n 7040) 0.1 v))
           (x2 (hpf x1 523))
           (m1 (add (mul3 t1 e1 0.25) (mul3 t2 e2 0.25)))
           (m2 (add (mul3 x1 e3 0.20) (mul3 x2 e4 0.20))))
      (add m1 m2))))

(define snare-909
  (let* ((x (mouse-x kr 1 4 linear 0.2))
         (y (mouse-y kr 0.25 0.75 exponential 0.2))
         (t (impulse kr (mul 3 x) 0))
         (n (white-noise ar))
         (v (t-rand 0.25 1.0 t)))
    (pan2 (snr t n v) 0 y)))

(hear snare-909)
