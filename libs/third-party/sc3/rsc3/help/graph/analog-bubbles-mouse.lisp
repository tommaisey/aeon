;; analog bubbles {mouse} (jmcc) #1

(define analog-bubbles
  (let* ((y (mouse-y kr 0.1 10 exponential 0.2))
         (x (mouse-x kr 2 40 exponential 0.2))
         (o (mul-add (lf-saw kr x 0) -3 80))
         (f (mul-add (lf-saw kr y 0) 24 o))
         (s (mul (sin-osc ar (midi-cps f) 0) 0.04)))
    (comb-n s 0.2 0.2 4)))

(hear analog-bubbles)
