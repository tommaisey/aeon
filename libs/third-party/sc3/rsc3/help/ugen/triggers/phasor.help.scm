; phasor controls sine frequency.
; end frequency matches a second sine wave.

(hear
 (let* ((r (mouse-x kr 0.2 2 1 0.1))
        (t (impulse ar r 0))
        (x (phasor ar t (fdiv r sample-rate) 0 1 0))
        (f (mce2 (lin-lin x 0 1 600 1000) 1000)))
   (mul (sin-osc ar f 0) 0.2)))
