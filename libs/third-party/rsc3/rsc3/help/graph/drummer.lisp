;; drummer (thor magnusson)

(define drummer
  (let* ((tempo 4)
         (n (white-noise ar))
         (tr (impulse ar tempo 0))
         (tr-2 (pulse-divider tr 4 2))
         (tr-4 (pulse-divider tr 4 0))
         (snare (mul n (decay2 tr-2 0.005 0.5)))
         (bass (mul (sin-osc ar 60 0) (decay2 tr-4 0.005 0.5)))
         (hihat (mul (hpf n 10000) (decay2 tr 0.005 0.5))))
    (pan2 (add3 snare bass hihat) 0 0.4)))

(hear drummer)
