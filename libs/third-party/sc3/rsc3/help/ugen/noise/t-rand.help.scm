(hear
 (let* ((t (dust kr (mce2 5 12)))
        (f (t-rand (mce2 200 1600) (mce2 500 3000) t)))
   (mul (sin-osc ar f 0) 0.2)))
