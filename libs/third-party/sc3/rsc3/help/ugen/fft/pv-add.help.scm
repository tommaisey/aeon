;; (pv-add bufferA bufferB)

;; Complex addition: RealA + RealB, ImagA + ImagB

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc 11 2048 1))
   (async fd (b-alloc-read 12 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((a (play-buf 1 12 (buf-rate-scale kr 12) 0 0 1))
       (f (fft* 10 a))
       (g (fft* 11 (mul (sin-osc ar 440 0) 0.2)))
       (h (pv-add f g)))
  (audition (out 0 (mul (ifft* h) 0.5))))

(audition
 (out 0 (mul (add (play-buf 1 12 (buf-rate-scale kr 12) 0 0 1)
		  (mul (sin-osc ar 440 0) 0.2))
	     0.5)))
