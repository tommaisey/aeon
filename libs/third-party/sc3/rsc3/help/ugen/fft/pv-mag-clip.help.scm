;; (PV_Magclip buffer threshold)

;; clip bins to a threshold.  clips bin magnitudes to a maximum
;; threshold.

;; buffer    - fft buffer.
;; threshold - magnitude threshold.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc-read 12 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((a (play-buf 1 12 (buf-rate-scale kr 12) 0 0 1))
       (f (fft* 10 a))
       (h (pv-mag-clip f (mouse-x kr 0 5 0 0.1))))
  (audition (out 0 (mul (ifft* h) 0.5))))
