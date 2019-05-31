;; (pv-local-max buffer threshold)

;; Pass bins which are a local maximum

;; Passes only bins whose magnitude is above a threshold and above
;; their nearest neighbors.

;; buffer - fft buffer.
;; threshold - magnitude threshold.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc-read 12 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((a (play-buf 1 12 (buf-rate-scale kr 12) 0 0 1))
       (f (fft* 10 a))
       (h (pv-local-max f (mouse-x kr 0 100 0 0.1))))
  (audition (out 0 (mul (ifft* h) 0.5))))
