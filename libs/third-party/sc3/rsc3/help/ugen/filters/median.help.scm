;; (median length in) 

;; median filter.

;; Signal with impulse noise.

(audition
 (out 0 (median 3 (add (mul (saw ar 500) 0.1) (mul (dust2 ar 100) 0.9)))))

;; The median length can be increased for longer duration noise.

(audition
 (out 0 (median 5 (add (mul (saw ar 500) 0.1) (lpz1 (mul (dust2 ar 100) 0.9))))))

;; Long median filters begin chopping off the peaks of the waveform

(audition
 (out 0 (let ((x (mul (sin-osc ar 1000 0) 0.2)))
	  (mce2 x (median 31 x)))))

;; Another noise reduction application. Use median filter for high
;; frequency noise.  Use leak-dc for low frequency noise.

(audition
 (out 0 (let* ((s0 (mul-add (white-noise ar) 0.1 (mul (sin-osc ar 800 0) 0.1)))
	       (s1 (median 31 s0)))
	  (leak-dc s1 0.9))))
