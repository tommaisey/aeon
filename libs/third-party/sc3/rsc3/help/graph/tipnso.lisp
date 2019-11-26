;; tipnso.scm (rd)

(define tipnso
  (let* ((t (impulse ar (mouse-x kr 1 32 0 0.1) 0))
         (e (decay2 t 0.01 (mce2 0.1 0.15)))
         (n (ti-rand 16 72 t))
         (f (midi-cps (add n (add 36 (mul 12 (ti-rand 0 1 t)))))))
    (mrg2 (mul (add (mul (sin-osc ar f 0) e)
                    (bpf (mul (pink-noise ar) e)
                         (add 36 (midi-cps n))
                         (fdiv 175 (midi-cps n))))
               (mce2 0.15 0.1))
          (send-trig t 0 n))))

(hear tipnso)
