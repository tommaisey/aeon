(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 0 "/home/rohan/data/audio/pf-c5.aif" 0 0))))

(audition (out 0 (buf-rd 1 ar 0 (mul (sin-osc ar 0.1 0) (buf-frames ir 0)) 0 2)))

(let ((phase (mul (lf-noise1 ar (mouse-x kr (mce2 5 10) 100 0 0.1))
		  (buf-frames ir 0))))
  (audition (out 0 (buf-rd-l 1 ar 0 phase 0))))

(let ((phase (add (lf-tri ar 0.1 0)
		  (mul (lf-tri ar 0.23 0) (buf-frames ir 0)))))
  (audition (out 0 (buf-rd-l 1 ar 0 phase 0))))

;; Use a phasor index into the file

(let ((phase (phasor ar
		     0
		     (mul (mouse-x kr 0.5 2 0 0.1)
			  (buf-rate-scale kr 0))
		     0
		     (buf-frames kr 0)
		     0)))
  (audition (out 0 (buf-rd 1 ar 0 phase 1 (mouse-y kr 0 5 0 0.1)))))
