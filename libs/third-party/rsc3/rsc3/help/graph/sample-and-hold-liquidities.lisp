;; sample and hold liquidities (jmcc) #2

(define sample-and-hold-liquidities
  (let* ((r (mouse-x kr 1 200 1 0.1))
         (t (recip r))
         (c (mul (impulse kr r 0) 0.4))
         (cf (mouse-y kr 100 8000 1 0.1))
         (f (latch (mul-add (white-noise kr) (mul cf 0.5) cf) c))
         (p (latch (white-noise kr) c))
         (e (decay2 c (mul 0.1 t) (mul 0.9 t)))
         (i (pan2 (mul (sin-osc ar f 0) e) p 1)))
    (comb-n i 0.3 0.3 2)))

(hear sample-and-hold-liquidities)
