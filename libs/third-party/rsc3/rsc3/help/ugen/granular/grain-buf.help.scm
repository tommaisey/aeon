(define g
  (letc ((gate 1)
         (amp 1)
         (sndbuf 0)
         (envbuf -1))
    (let* ((x (mouse-x kr -1 1 0 0.1))
           (y (mouse-y kr 10 45 0 0.1))
           (i (impulse kr y 0))
           (r (lin-lin (lf-noise1 kr 500) -1 1 0.5 2))
           (p (lin-lin (lf-noise2 kr 0.1) -1 1 0 1)))
      (out 0 (grain-buf 2 i 0.1 sndbuf r p 2 x envbuf)))))

(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 10 "/home/rohan/audio/metal.wav" 0 0))
   (send-synth fd "g" g)
   (send fd (s-new2 "g" -1 add-to-tail 1 "sndbuf" 10 "envbuf" -1)))))
