;; (pv-copyPhase bufferA bufferB)

;; Combines magnitudes of first input and phases of the second input.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc 11 2048 1))
   (async fd (b-alloc-read 12 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((a (mul (white-noise ar) 0.2))
       (b (mul (sin-osc ar 100 0) 0.2))
       (f (fft* 10 a))
       (g (fft* 11 b))
       (h (pv-copy-phase f g)))
  (audition (out 0 (mul (ifft* h) 0.5))))

(let* ((a (mul (white-noise ar) 0.2))
       (b (play-buf 1 12 (buf-rate-scale kr 12) 0 0 1))
       (f (fft* 10 a))
       (g (fft* 11 b))
       (h (pv-copy-phase f g)))
  (audition (out 0 (mul (ifft* h) 0.5))))
