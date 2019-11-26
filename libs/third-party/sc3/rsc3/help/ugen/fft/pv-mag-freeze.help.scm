;; (PV_Magfreeze buffer freeze)

(define fn "/home/rohan/data/audio/instr/crotales/crotale05(D).wav")

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc-read 12 fn 0 0))))

(let ((dup (lambda (a) (mce2 a a)))
      (s (sin-osc ar (mul-add (lf-noise1 kr 5.2) 250 400) 0))
      (f (sin-osc kr 0.2 0)))
  (audition (out 0 (dup (mul 0.25 (ifft* (pv-mag-freeze (fft* 10 s) f)))))))

(let ((dup (lambda (a) (mce2 a a)))
      (s (play-buf 1 ar 12 (buf-rate-scale kr 12) 1 0 1 0))
      (f (u:gt (mouse-y kr 0 1 0 0.1) 0.5)))
  (audition (out 0 (dup (mul 0.25 (ifft* (pv-mag-freeze (fft* 10 s) f)))))))
