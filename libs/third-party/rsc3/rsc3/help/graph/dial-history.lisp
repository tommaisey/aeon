;; dial history (jrhb)

(define range
  (lambda (s l r)
    (lin-lin s 0 1 l r)))

(define mce-r
  (lambda (l)
    (make-mce (map make-mce l))))

(define mce-mrg
  (lambda (u)
    (mrg-n (mce-channels u))))

(define dial-history
  (let* ((mfv (transpose (list (list 697 770 852 941)
                               (list 1209 1336 1477 1633))))
         (numbers (transpose (list (list 3 0 0 0 1 1 1 2 2 2)
                                   (list 1 0 1 2 0 1 2 0 1 2))))
         (n (dwhite dinf 7 12))
         (w (dwhite 1 2 7))
         (b (dbrown n 0.1 0.2 0.01))
         (rt (dseq dinf (mce2 w b)))
         (q (dseq dinf (make-mce (enum-from-to 1 10))))
         (g1 (gray-noise ar))
         (g2 (gray-noise ar))
         (d (lfd-noise3 kr 0.5))
         (tr (trig (t-duty kr rt 0 do-nothing q 1) 0.09))
         (pat (latch tr tr))
         (x (mouse-x kr 0 1 linear 0.2))
         (h (hasher (mul pat x)))
         (which (trunc (range h 0 (length numbers)) 1))
         (both (select which (mce-r numbers)))
         (dial (select both (mce-r mfv)))
         (sig (mul3 (sin-osc ar dial 0) 0.05 tr))
         (dsig (delay-n sig 0.2 (range d 0 0.01)))
         (hiss (mul-add g1 0.01 (hpf (mul g2 0.02) 3000))))
    (add dsig hiss)))

(hear dial-history)
