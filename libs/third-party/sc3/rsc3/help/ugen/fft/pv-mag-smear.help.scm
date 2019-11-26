;; (pv-mag-smear buffer bins)

;; Average a bin's magnitude with its neighbors. 

;; buffer - fft buffer.

;; bins   - number of bins to average on each side of bin. 
;;          As this number rises, so will CPU usage.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc-read 12 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((dup (lambda (a) (mce2 a a)))
       (in (mul (lf-saw ar 500 0) (decay2 (mul (impulse ar 2 0) 0.2) 0.01 2)))
       (c0 (fft* 10 in))
       (c1 (pv-mag-smear c0 (mouse-x kr 0 100 0 0.1))))
  (audition (out 0 (mul 0.5 (dup (ifft* c1))))))

(let* ((dup (lambda (a) (mce2 a a)))
       (s (play-buf 1 12 (buf-rate-scale kr 12) 1 0 1))
       (x (mouse-x kr 0 100 0 0.1)))
  (audition (out 0 (mul 0.5 (dup (ifft* (pv-mag-smear (fft* 10 s) x)))))))
