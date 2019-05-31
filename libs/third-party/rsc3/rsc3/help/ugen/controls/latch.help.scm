(hear (mul (blip ar (mul-add (latch (white-noise ar) (impulse ar 9 0)) 400 500) 4) 0.2))

;; The above is just meant as example. lf-noise0 is a faster way to
;; generate random steps :

(hear (mul (blip ar (mul-add (lf-noise0 kr 9) 400 500) 4) 0.2))

;; http://create.ucsb.edu/pipermail/sc-users/2006-December/029991.html

(hear
 (let* ((n0 (mul-add (lf-noise2 kr 8) 200 300))
        (n1 (mul-add (lf-noise2 kr 3) 10 20))
        (s (blip ar n0 n1))
        (x (mouse-x kr 1000 (mul sample-rate 0.1) 1 0.1)))
   (latch s (impulse ar x 0))))
