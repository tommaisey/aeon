;; (disk-in num-channels rate bufnum)

;; Continously play a soundfile from disk. This requires a buffer to
;; be preloaded with one buffer size of sound.  The buffer size must
;; be a multiple of twice the synth block size. The default block size
;; is 64.

;; Note that disk-in reads the number of outputs to create from what
;; looks like an input, but it is not an input, and cannot be set
;; using a control.

(let ((f "/home/rohan/audio/metal.wav")
      (n 1))
  (with-sc3
   (lambda (fd)
     (async fd (b-alloc 0 8192 n))
     (async fd (b-read 0 f 0 -1 0 1))
     (play fd (out 0 (disk-in n ar 0))))))

(with-sc3 reset)

(with-sc3
 (lambda (fd)
   (async fd (b-close 0))
   (async fd (b-free 0))))
