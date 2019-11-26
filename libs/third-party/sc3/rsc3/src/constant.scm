;; MATH

(define dinf 9e8)
(define e 2.718281828459045) ;; (exp 1.0)
(define pi 3.141592653589793) ;; (* 4.0 (atan 1.0))
(define two-pi (* 2 pi))
(define half-pi (* 0.5 pi))

;; ENUMERATION

(define add-to-head 0)
(define add-to-tail 1)
(define add-before 2)
(define add-after 3)
(define add-replace 4)

(define gen-normalize 1)
(define gen-wavetable 2)
(define gen-clear 4)

(define do-nothing 0)
(define pause-synth 1)
(define remove-synth 2)
(define remove-group 14)

(define no-loop 0)
(define loop 1)

(define linear 0)
(define exponential 1)
