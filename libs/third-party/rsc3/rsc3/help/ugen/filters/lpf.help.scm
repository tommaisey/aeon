;; (lpf in freq)

;; Second order Butterworth lowpass filter.

(audition
 (let ((f (x-line kr 0.7 300 20 remove-synth)))
   (out 0 (lpf (mul (saw ar 200) 0.1)
	       (mul-add (f-sin-osc kr f 0) 3600 4000)))))
