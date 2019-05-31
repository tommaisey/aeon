(define quad_ quad-c)

(hear (mul (quad_ ar 4000 1 -1 -0.75 0) 0.1))

(hear
 (let ((r (mouse-x kr 3.5441 4 0 0.1)))
   (mul (quad_ ar 4000.0 (neg r) r 0 0.1) 0.1)))

(hear
 (let ((r (mouse-x kr 3.5441 4 0 0.1)))
   (mul (sin-osc ar (mul-add (quad_ ar 4 (neg r) r 0 0.1) 800 900) 0) 0.1)))
