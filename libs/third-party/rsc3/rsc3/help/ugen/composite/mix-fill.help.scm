;; (mix-fill n f)

(hear
 (let ((n 6)
       (o (lambda (_) (mul (f-sin-osc ar (rand 200 700) 0) 0.05))))
  (mix-fill n o)))
