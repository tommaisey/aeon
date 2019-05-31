;; (pv-phase-shift buffer shift)

;; add `shift' to the phase component of every bin at `buffer'.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))))

(let ((n (mul (white-noise ar) 0.1))
      (x (mouse-x kr 0 1 0 0.1)))
  (audition (out 0 (ifft* (pv-phase-shift (fft* 10 n) x)))))
