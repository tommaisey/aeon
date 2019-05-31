;; (buf-frames rate bufnum)

;; Current duration of buffer.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 0 "/home/rohan/audio/metal.wav" 0 0))))

(let ((p (phasor ar 0 (buf-rate-scale kr 0) 0 (buf-frames kr 0) 0)))
  (audition (out 0 (buf-rd 1 ar 0 p 0 2))))

(let ((p (k2a (mouse-x kr 0 (buf-frames kr 0) 0 0.1))))
  (audition (out 0 (buf-rd 1 ar 0 p 0 2))))
