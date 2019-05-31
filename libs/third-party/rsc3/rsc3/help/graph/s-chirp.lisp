;; s-chirp (rd)

(define s-chirp
  (let* ((b (as-local-buf (list 0 2 3.2 5 7 9 10)))
         (x (mouse-x kr 15 0 0 0.1))
         (y (mouse-y kr 15 27 0 0.1))
         (t (dust kr 9))
         (c (t-choose t (make-mce (list 36 48 60 72))))
         (n (mul (lf-noise1 kr (mce2 3 3.05)) 0.04))
         (d (ti-rand x y t))
         (e (decay2 t 0.005 (t-rand 0.02 0.15 t)))
         (k (degree-to-key b d 12))
         (f (midi-cps (add3 c k n)))
         (m (mul3 e (sin-osc ar f 0) 0.2))
         (u (pulse-divider t 9 0))
         (r0 (t-rand 0.0075 0.125 u))
         (r1 (t-rand 0.05 0.15 u)))
    (mul-add m 0.5 (allpass-c m 0.15 r0 r1))))

(hear s-chirp)
