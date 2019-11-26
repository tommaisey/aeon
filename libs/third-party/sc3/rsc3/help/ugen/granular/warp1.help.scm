(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 10 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((p (lin-lin (lf-saw kr 0.05 0) -1 1 0 1))
       (x (mouse-x kr 0.5 2 0 0.1)))
  (warp1 1 10 p x 0.1 -1 8 0.1 2))
