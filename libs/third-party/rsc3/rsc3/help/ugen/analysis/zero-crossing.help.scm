;; (zero-crossing in)

;; Zero crossing frequency follower.

;; outputs a frequency based upon the distance between interceptions
;; of the X axis. The X intercepts are determined via linear
;; interpolation so this gives better than just integer wavelength
;; resolution. This is a very crude pitch follower, but can be useful
;; in some situations.

;; in - input signal.

(let* ((a (mul (sin-osc ar (mul-add (sin-osc kr 1 0) 600 700) 0) 0.1))
       (b (mul (impulse ar (zero-crossing a) 0) 0.25)))
  (audition (out 0 (mce2 a b))))
