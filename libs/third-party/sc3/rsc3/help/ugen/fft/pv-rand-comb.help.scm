;; (pv-rand-comb buffer wipe trig)

;; randomly clear bins.

;; buffer = fft buffer.  wipe = clear bins from input in a random
;; order (0, 1).  trig = select new random ordering.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))))

(let ((dup (lambda (a) (mce2 a a)))
      (n (mul (white-noise ar) 0.5))
      (x (mouse-x kr 0.6 0.95 0 0.1))
      (t (impulse kr 0.4 0)))
  (audition (out 0 (dup (ifft* (pv-rand-comb (fft* 10 n) x t))))))
