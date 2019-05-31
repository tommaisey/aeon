;; (lag3 in lagTime)

;; lag3 is the same as (lag (Lag (Lag in time) time) time).

(let* ((x (mouse-x kr 220 440 0 0.1))
       (f (mce2 x (lag3 x 1))))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
