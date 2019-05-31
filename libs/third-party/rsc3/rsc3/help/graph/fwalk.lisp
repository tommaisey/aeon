;; fwalk (rd)

(define fwalk-i
  (lambda (r)
    (let* ((t (dust kr 3))
           (r1 (ti-rand 0 6 t))
           (r2 (t-rand -0.0001 0.0001 t))
           (f (buf-rd-l 1 kr (mce2 0 1) r1 no-loop))
           (o1 (blip ar (midi-cps (add r f)) 12))
           (o2 (blip ar (midi-cps (add3 r f r2)) 12)))
      (mul3 (add o1 o2) (decay2 t 0.3 1.2) 0.1))))

(define fwalk
  (let* ((n (list 40.0 47.0 42.0 40.0 50.0
                  43.0 35.0 43.0 40.0 47.0
                  45.0 35.0 43.0 42.0 59.0
                  48.0 40.0 47.0 52.0 45.0))
         (m (list 40.0 40.0 42.0 47.0 50.0
                  35.0 43.0 43.0 40.0 45.0
                  42.0 35.0 48.0 47.0 43.0
                  40.0 59.0 45.0 47.0 52.0))
         (a (map (lambda (b) (b-alloc b 20 1)) (list 0 1)))
         (s (zip-with (lambda (b d) (b-setn1 b 0 d)) (list 0 1) (list n m))))
    (lambda (fd)
      (begin
        (for-each (lambda (m) (async fd m)) a)
        (for-each (lambda (m) (send fd m)) s)
        (play fd (out 0 (add (fwalk-i 24) (fwalk-i 36))))))))

(with-sc3 fwalk)
