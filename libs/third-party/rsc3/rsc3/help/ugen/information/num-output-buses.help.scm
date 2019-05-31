;; num-output-buses

;; The number of audio buses allocated to output.

(let ((bus num-output-buses))
  (audition (out 0 (mul (pulse ar 90 0.3)
			(amplitude kr (lag (in 1 ar bus) 0.1) 0.01 0.01)))))
