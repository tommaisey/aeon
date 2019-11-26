;; (pv-mag-above buffer threshold)

;; Pass only bands where the magnitude is above `threshold'.  This
;; value is not normalized and is therefore dependant on the buffer
;; size.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc 11 2048 1))
   (async fd (b-alloc-read 12 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((a (play-buf 1 12 (buf-rate-scale kr 12) 0 0 1))
       (x (mouse-x kr 1 100 0 0.1))
       (y (mouse-y kr 0 1 0 0.1))
       (c1 (fft* 10 a))
       (c2 (pv-copy c1 11))
       (c3 (pv-mag-below c1 x))
       (c4 (pv-mag-above c2 x)))
  (audition (out 0 (mul (mce2 (ifft* c3) (ifft* c4)) (mce2 y (sub 1 y))))))

(let* ((f1 (squared (mul-add (sin-osc kr 0.08 0) 6 6.2)))
       (f2 (mul-add (sin-osc kr f1 0) 100 800))
       (s (sin-osc ar f2 0))
       (x (mouse-x kr 1 1024 0 0.1))
       (y (mouse-y kr 0 1 0 0.1))
       (c1 (fft* 10 s))
       (c2 (pv-copy c1 11))
       (c3 (pv-mag-below c1 x))
       (c4 (pv-mag-above c2 x)))
  (audition (out 0 (mul (mce2 (ifft* c3) (ifft* c4)) (mce2 y (sub 1 y))))))
