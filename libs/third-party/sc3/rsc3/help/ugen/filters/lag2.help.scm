;; (lag2 in lagTime)

;; lag2 is the same as (lag kr (Lag kr in time) time).

(let* ((x (mouse-x kr 220 440 0 0.1))
       (f (mce2 x (lag2 x 1))))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
