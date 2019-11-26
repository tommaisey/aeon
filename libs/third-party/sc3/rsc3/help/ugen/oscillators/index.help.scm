(import (sosc) (rsc3))

;; Allocate and set values at buffer 10.
(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 6 1))
   (send fd (b-setn1 10 0 (list 50 100 200 400 800 1600)))))

;; Index into the above buffer for frequency values.
(let ((f (mul (index 10 (mul (lf-saw kr 2 3) 4)) (mce2 1 9))))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))

;; Free buffer
(with-sc3
 (lambda (fd)
   (async fd (b-free 10))))
