;; (free-verb in mix room damp)
;; (free-verb2 in1 in2 mix room damp)

;; A simple reverb.

;; in, in1, in2 - input signal
;;          mix - dry/wet balance (0,1)
;;         room - room size (0,1)
;;         damp - reverb high frequency damping (0,1)

(let* ((i (impulse ar 1 0))
       (c (lf-cub ar 1200 0))
       (s (mul3 (decay i 0.25) c 0.1))
       (x (mouse-x kr 0 1 0 0.1))
       (y (mouse-y kr 0 1 0 0.1))
       (r (free-verb s y x 0.5)))
  (audition (out 0 r)))

(let* ((i (sound-in (mce2 0 1)))
       (c (lambda (u n) (mce-channel u n)))
       (x (mouse-x kr 0 1 0 0.1))
       (y (mouse-y kr 0 1 0 0.1))
       (r (free-verb2 (c i 0) (c i 1) y x 0.5)))
  (audition (out 0 r)))
