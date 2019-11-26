;; (is-positive a)

;; Predicate to determine if a value is positive.

(define (is-positive a)
  (ge a 0.0))

(let* ((a (line ar -1 1 1 remove-synth))
       (b (is-positive a))
       (f (mul-add (mce2 a b) 600 900)))
  (audition 
   (out 0 (mul (sin-osc ar f 0) 0.1))))
