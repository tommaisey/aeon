;; (am-clip a b)

;; Two quadrant multiply, 0 when b <= 0, a*b when b > 0

(audition
 (out 0 (am-clip (white-noise ar)
                 (mul (f-sin-osc kr 1 0) 0.2))))
