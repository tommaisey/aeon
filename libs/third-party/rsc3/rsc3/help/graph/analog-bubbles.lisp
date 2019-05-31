;; analog bubbles (jmcc) #1

(define analog-bubbles
  (let* ((o (mul-add (lf-saw kr (mce2 8 7.23) 0) 3 80))
         (f (mul-add (lf-saw kr 0.4 0) 24 o))
         (s (mul (sin-osc ar (midi-cps f) 0) 0.04)))
    (comb-n s 0.2 0.2 4)))

(hear analog-bubbles)
