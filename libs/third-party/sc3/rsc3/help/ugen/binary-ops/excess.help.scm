;; (excess a b)

;; clipping residual.  Returns the difference of the original signal
;; and its clipped form: (a - clip2(a,b)).

(audition
   (out 0 (excess (f-sin-osc ar 1000 0) (line kr 0 1 8 do-nothing))))

(audition
 (out 0 (let ((a (f-sin-osc ar 1000 0))
	      (b (line kr 0 1 8 do-nothing)))
	  (sub a (clip2 a b)))))
