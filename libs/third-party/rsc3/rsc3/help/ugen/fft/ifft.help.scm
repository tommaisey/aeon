;; (Ifft buffer wintype)
;; (ifft* b) => (ifft b 0) 

;; inverse Fast Fourier Transform.  The inverse fast fourier transform
;; converts from frequency content to a signal.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 0 2048 1))))

(let* ((s (mul (white-noise ar) 0.05))
       (c (fft 0 s 0.5 0 1)))
  (audition (out 0 (Ifft c 0))))
