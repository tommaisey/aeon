;; (buf-rate-scale rate bufnum)

;; Buffer rate scaling in respect to server samplerate.  Returns a
;; ratio by which the playback of a soundfile is to be scaled.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 0 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((r (mul (rand 0.5 2) (buf-rate-scale kr 0)))
       (p (phasor ar 0 r 0 (buf-frames kr 0) 0)))
  (audition (out 0 (buf-rd 1 ar 0 p 0 2))))
