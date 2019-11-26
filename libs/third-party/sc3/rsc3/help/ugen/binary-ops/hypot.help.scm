;; (hypot x y)

;; Returns the square root of the sum of the squares of a and b. Or
;; equivalently, the distance from the origin to the point (x, y).

(audition
   (out 0 (mul (sin-osc ar 440 0)
	       (hypot (mouse-x kr 0 0.1 0 0.1) 
		      (mouse-y kr 0 0.1 0 0.1)))))

;; Object travels 200 meters in 6 secs (=120kph) passing 10 meters
;; from the listener.  The speed of sound is 344 meters/sec.

(let* ((x 10)
       (y (mul (lf-saw kr 1/6 0) 100))
       (distance (hypot x y))
       (velocity (slope distance))
       (pitch-ratio (fdiv (sub 344 velocity) 344)) 
       (amplitude (fdiv 10 (squared distance))))
  (audition
   (out 0 (mul (f-sin-osc ar (mul 1000 pitch-ratio) 0)
	       amplitude))))

(let* ((x 10)
       (y (mul (lf-saw kr 1/6 0) 100))
       (distance (hypot x y))
       (amplitude (fdiv 40 (squared distance)))
       (sound (rlpf (mul (f-sin-osc ar 200 0) (lf-pulse ar 31.3 0 0.4)) 400 0.3)))
  (audition
   (out 0 (mul (delay-l sound 110/344 (fdiv distance 344))
	       amplitude))))
