;; (limiter input level lookAheadTime)
 
;; peak limiter.  Limits the input amplitude to the given
;; level. limiter will not overshoot like compander will, but it needs
;; to look ahead in the audio. Thus there is a delay equal to twice
;; the lookAheadTime.  limiter, unlike compander, is completely
;; transparent for an in range signal.

(let* ((t (impulse ar 8 (mul (lf-saw kr 0.25 -0.6) 0.7)))
       (i (mul (decay2 t 0.001 0.3) (f-sin-osc ar 500 0))))
  (audition (out 0 (mce2 (mul i 0.1) (limiter i 0.2 0.01)))))
