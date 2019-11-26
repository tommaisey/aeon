;; (delay-n in maxDelayTime delayTime)
;; (delay-l in maxDelayTime delayTime)
;; (delay-c in maxDelayTime delayTime)

;; Simple delay line.  There are three forms, delay-n uses no
;; interpolation, delay-l uses linear interpolation, delay-c uses cubic
;; interpolation.  The maximum delay length is set at initialization
;; time and cannot be extended.

;; dust randomly triggers decay to create an exponential decay
;; envelope for the white-noise input source.

(let ((z (mul (decay (dust ar 1) 0.3)
	      (white-noise ar))))
  (audition
   (out 0 (add (delay-n z 0.2 0.2) z))))
