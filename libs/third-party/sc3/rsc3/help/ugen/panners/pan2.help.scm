(hear
 (let ((p (f-sin-osc kr 2 0)))
   (pan2 (pink-noise ar) p 0.3)))

(hear
 (let ((x (mouse-x kr -1 1 0 0.1))
       (y (mouse-y kr 0 0.1 0 0.1)))
   (pan2 (pink-noise ar) x y)))
