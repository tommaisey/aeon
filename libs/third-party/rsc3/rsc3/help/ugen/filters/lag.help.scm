;; (lag in lagTime)

;; A simple averaging filter.

(let* ((x (mouse-x kr 220 440 0 0.1))
       (f (mce2 x (lag x 1))))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
