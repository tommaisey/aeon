;; (buf-dur rate bufnum)

;; Current duration of buffer.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 0 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((t (impulse ar (recip (buf-dur kr 0)) 0))
       (p (sweep t (buf-sample-rate kr 0))))
  (audition (out 0 (buf-rd 1 ar 0 p 0 2))))

