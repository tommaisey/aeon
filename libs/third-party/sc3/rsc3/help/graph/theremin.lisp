;; theremin (jmcc)

(define theremin
  (let* ((mod 7)
         (detune 0)
         (x (mouse-x kr 0 0.9 0 0.2))
         (y (mouse-y kr 4000 200 1 0.8))
         (f (add y detune))
         (f* (add f (mul3 f (sin-osc ar mod 0) 0.02)))
         (a (mul (sin-osc ar f* 0) x)))
    (pan2 a 0 1)))

(hear theremin)
