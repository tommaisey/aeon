;; No decay

(hear
 (let* ((s (mul (dust ar 20) (line kr 0 1 4 do-nothing)))
        (f (mul-add (peak-follower s 1.0) 1500 200)))
   (mul (sin-osc ar f 0) 0.2)))

;; A little decay

(hear
 (let* ((s (mul (dust ar 20) (line kr 0 1 4 do-nothing)))
        (f (mul-add (peak-follower s 0.999) 1500 200)))
   (mul (sin-osc ar f 0) 0.2)))

;; Mouse x controls decay

(hear
 (let* ((x (mouse-x kr 0.99 1.0001 1 0.1))
        (s (mul (dust ar 20) (line kr 0 1 4 do-nothing)))
        (f (mul-add (peak-follower s (u:min x 1.0)) 1500 200)))
   (mul (sin-osc ar f 0) 0.2)))

;; Follow a sine lfo

(hear
 (let* ((x (mouse-x kr 0.99 1.0001 1 0.1))
        (s (sin-osc kr 0.2 0))
        (f (mul-add (peak-follower s (u:min x 1.0)) 200 500)))
   (mul (sin-osc ar f 0) 0.2)))
