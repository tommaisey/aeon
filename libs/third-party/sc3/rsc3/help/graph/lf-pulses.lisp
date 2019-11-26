;; lf pulses (rd)

(define lf-pulses
  (let* ((n0 (lf-noise0 ar (mce2 20 40)))
         (n1 (lf-noise0 ar (mce2 5 10)))
         (x (mouse-x kr 0.012 0.19 1 0.1))
         (f (formlet (blip ar 10 12) (mul-add n0 43 700) 0.005 x))
         (o (mul (sin-osc ar 40 0) n1)))
    (clip2 (leak-dc (add f o) 0.995) 0.75)))

(hear lf-pulses)
