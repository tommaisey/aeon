;; shifting pulses (rd)

(define wrp
  (lambda (i l r)
    (lin-lin i -1 1 l r)))

(define prt
  (lambda (f a)
    (let ((f* (mul f (mul (wrp (sin-osc kr (rand 0.2 0.9) 0) 1 1.01)
                          (rand 0.95 1.05)))))
      (mul (sin-osc ar (mce2 f f*) 0)
           (mul a (clone 2 (rand 0.95 1.05)))))))

;; (hear (prts 2 900 0.002))
;; (hear (prts 9 40 0.006))
(define prts
  (lambda (n f a)
    (mix
     (make-mce
      (map
       (lambda (f)
         (prt f a))
       (enum-from-then-to f (+ f f) (* n f)))))))

(define shifting-pulses
  (let* ((n1 (clone 2 (brown-noise kr)))
         (n2 (clone 2 (brown-noise kr)))
         (n3 (clone 2 (brown-noise kr)))
         (t (dust kr 0.75))
         (l (latch t t))
         (p (mul (pulse ar (wrp n1 2 (mce2 11 15)) 0.01) 0.1))
         (f (wrp n2 300 1800))
         (rq (wrp n3 0.01 2)))
    (mrg2
     (add4
      (prts 2 900 0.002)
      (prts 9 40 0.006)
      (mul (formant ar (mce2 20 21) (wrp (lf-noise2 kr 2) 10 100) 200) 0.35)
      (mul l (rlpf p f rq)))
     (send-trig t 0 t))))

(hear shifting-pulses)
