(hear
 (let ((b (as-local-buf (list 200 300 400 500 600 800)))
       (f (wrap-index b (mouse-x kr 0 18 0 0.1))))
   (mul (sin-osc ar f 0) 0.1)))
