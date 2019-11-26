;; feedr (rd)
;; warning: input/output feedback loop

(import (sosc) (rsc3) (rhs))

(define delay-wr
  (lambda (b in)
    (record-buf ar b 0 1 0 1 loop 0 do-nothing in)))

(define tap
  (lambda (nc b delay-time)
    (play-buf nc ar b 1 0 (mul delay-time (neg sample-rate)) 1 do-nothing)))

;; (feedr 6 2 (mce* 0 1))
(define feedr
  (lambda (dl nc ch)
    (let* ((n 18)
           (t (replicate-m n (rand 0.0 dl)))
           (g (replicate-m n (rand 0.4 1.0)))
           (f (replicate-m n (rand 0.9 0.95)))
           (d (zip-with
               (lambda (t g)
                 (mul (tap nc 10 t) g))
               t g))
           (x (mouse-x kr 0.02 1.0 1 0.1)))
      (make-mrg
       (out 0 (clip2 (leak-dc (hpf (foldl1 add d) 20) 0.995) 1))
       (delay-wr 10 (foldl add
                           (sound-in ch)
                           (map
                            (lambda (e)
                              (mul e x))
                            (zip-with mul d f))))))))

(define run
  (lambda (fd)
    (let* ((dl 6)
           (ff (* dl (server-sample-rate-actual fd)))
           (nc 2))
      (begin
        (send fd (b-alloc 10 ff nc))
        (audition (feedr dl nc (mce* 0 1)))))))

(with-sc3 run)

(with-sc3
 (lambda (fd)
   (send fd (b-zero 10))))
