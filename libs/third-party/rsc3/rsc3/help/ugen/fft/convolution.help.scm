(hear
 (let ((input (sound-in (mce2 0 1)))
       (kernel (white-noise ar)))
   (mul (convolution input kernel 2048) 0.1)))

(let ((a 2048)
      (b 0))
  (with-sc3
   (lambda (fd)
     (async fd (b-alloc b a 1))
     (send fd (b-set1 b 0 1.0))
     (replicate-m 100 (send fd (b-set1 b (random-integer a) (random 0.0 1.0))))
     (play fd (out 0 (mul (convolution
			   (sound-in (mce2 0 1))
			   (play-buf 1 b (buf-rate-scale kr b) 1 0 1)
			   (* 2 a))
			  0.2))))))
