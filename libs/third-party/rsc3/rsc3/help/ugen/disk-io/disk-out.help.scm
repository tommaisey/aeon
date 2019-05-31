;; (disk-out bufnum channels)

;; Note: There are constraints on the size of the buffer, it must be
;; greater than or equal to twice the size of the audio bus.  There
;; must be the same number of channels at the buffer and the disk-out
;; ugen.

(let ((bus-size 1024)
      (bufferexpt 15))
  (= 0 (fxand (expt 2 bufferexpt)
	      (- (fxarithmetic-shift bus-size 1) 1))))

(let ((g (letc ((bufnum 0))
	   (let ((z (clip2
		     (rlpf 
		      (lf-pulse ar 
				(mul-add (sin-osc kr 0.2 0) 10 21)
				(mce2 0 0.1) 
				0.1) 
		      100 
		      0.1) 
		     0.4)))
	     (mrg2 (disk-out bufnum z)
		   (out 0 z))))))
  (with-sc3
   (lambda (fd)
     (send-synth fd "disk-out-help" g)
     (async fd (b-alloc 10 32768 2))
     (async fd (b-write 10
		       "/tmp/test.aiff"
		       "aiff"
		       "float"
		       32768
		       0
		       1))
     (send fd (s-new1 "disk-out-help" 1001 1 1 "bufnum" 10)))))

(with-sc3
 (lambda (fd)
   (send fd (n-free1 1001))
   (async fd (b-close 10))
   (async fd (b-free 10))))

(system "sndfile-info /tmp/test.aiff")
(system "jack.play /tmp/test.aiff")
