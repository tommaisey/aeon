;; (Atan2 x y)

;; Returns the arctangent of y/x.

;; See also hypot.

;; add a pan to the hypot doppler examples by using atan2 to find the
;; azimuth, or direction angle, of the sound source.  Assume speakers
;; at +/- 45 degrees and clip the direction to between those.

(let* ((x 10)
       (y (mul (lf-saw kr 1/6 0) 100))
       (distance (hypot x y))
       (amplitude (fdiv 40 (squared distance)))
       (sound (rlpf (mul (f-sin-osc ar 200 0) (lf-pulse ar 31.3 0 0.4)) 400 0.3))
       (azimuth (atan2 y x))
       (loc (clip2 (fdiv azimuth (/ pi 2)) 1)))
  (audition
   (out 0 (pan2 (delay-l sound 110/344 (fdiv distance 344))
		loc
		amplitude))))
