(define src (mul (white-noise ar) 0.01))
(define ctl (x-line kr 0.0001 0.01 20 remove-synth))

(hear (comb-n src 0.01 ctl 0.2))
(hear (comb-l src 0.01 ctl 0.2))
(hear (comb-c src 0.01 ctl 0.2))

;; With negative feedback:

(hear (comb-n src 0.01 ctl -0.2))
(hear (comb-l src 0.01 ctl -0.2))
(hear (comb-c src 0.01 ctl -0.2))

;; Used as an echo.

(hear (comb-n (mul (decay (mul (dust ar 1) 0.5) 0.2) (white-noise ar)) 0.2 0.2 3))
