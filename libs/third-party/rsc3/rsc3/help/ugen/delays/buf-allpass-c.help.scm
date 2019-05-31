(with-sc3
 (lambda (fd)
   (async fd (b-alloc 0 44100 1))))

(let ((x (mul3 (decay (dust ar 1) 0.2) (white-noise ar) 0.5)))
  (audition (out 0 (buf-allpass-n 0 x 0.25 6))))
