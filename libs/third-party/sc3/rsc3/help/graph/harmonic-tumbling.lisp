;; harmonic tumbling (jmcc) #1

(define harmonic-tumbling
  (let* ((f 80)
         (p 10)
         (t (x-line kr (mce2 10 11) 0.1 60 0))
         (o (lambda (h)
              (let* ((n (dust kr t))
                     (r (rand 0.25 0.5))
                     (e (decay2 (mul n 0.02) 0.005 r)))
                (mul (f-sin-osc ar (* f (+ h 1)) 0) e)))))
    (mix-fill p o)))

(hear harmonic-tumbling)
