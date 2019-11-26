;; (pv-bin-scramble buffer wipe width trig)

;; randomizes the order of the bins.  The trigger will select a new
;; random ordering.

;; buffer - fft buffer.
;; wipe   - scrambles more bins as wipe moves from zero to one.
;; width  - a value from zero to one, indicating the maximum randomized
;;          distance of a bin from its original location in the spectrum.
;; trig   - a trigger selects a new random ordering.

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 2048 1))
   (async fd (b-alloc-read 12 "/home/rohan/audio/metal.wav" 0 0))))

(let* ((a (play-buf 1 12 (buf-rate-scale kr 12) 1 0 1))
       (f (fft* 10 a))
       (g (pv-bin-scramble f
			  (mouse-x kr 0.0 1.0 0 0.1)
			  (mouse-y kr 0.0 1.0 0 0.1)
			  (impulse kr 4 0)))
       (h (ifft* g)))
  (audition (out 0 (mul 0.5 (mce2 h h)))))

;; careful - reads adc!
(let* ((a (mul (sound-in (mce2 0 1)) 4.0))
       (f (fft* 10 a))
       (g (pv-bin-scramble f
			  (mouse-x kr 0.25 1.0 0 0.1)
			  (mouse-y kr 0.25 1.0 0 0.1)
			  (impulse kr (mul-add (lf-noise0 kr 2) 8 10) 0)))
       (h (ifft* g)))
  (audition (out 0 (pan2 h 0 0.5))))
