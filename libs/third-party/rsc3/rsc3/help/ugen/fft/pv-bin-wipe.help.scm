;; (pv-bin-wipe bufferA bufferB wipe)

;; Combine low and high bins from two inputs

;; Copies low bins from one input and the high bins of the other.

;; bufferA - fft buffer A.
;; bufferB - fft buffer B.
;; wipe - can range between -1 and +1.

;; if wipe == 0 then the output is the same as inA.
;; if  wipe > 0 then it begins replacing with bins from inB from the bottom up.
;; if  wipe < 0 then it begins replacing with bins from inB from the top down.

(with-sc3
 (lambda (fd)
  (async fd (b-alloc 10 2048 1))
  (async fd (b-alloc 11 2048 1))
  (async fd (b-alloc-read 12 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((a (mul (white-noise ar) 0.2))
       (b (mul (sin-osc ar 100 0) 0.2))
       (f (fft* 10 a))
       (g (fft* 11 b))
       (h (pv-bin-wipe f g (mouse-x kr -1 1 0 0.1))))
  (audition (out 0 (mul (ifft* h) 0.5))))

(let* ((a (mul (white-noise ar) 0.2))
       (b (play-buf 1 12 (buf-rate-scale kr 12) 0 0 1))
       (f (fft* 10 a))
       (g (fft* 11 b))
       (h (pv-bin-wipe f g (mouse-x kr -1 1 0 0.1))))
  (audition (out 0 (mul (ifft* h) 0.5))))
