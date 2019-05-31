(with-sc3
 (lambda (fd)
   (async fd (b-alloc 0 44100 1))))

(let ((z (mul3 (decay (dust ar 1) 0.5) 0.3 (white-noise ar))))
  (audition (out 0 (add (buf-delay-c 0 z 0.2) z))))
