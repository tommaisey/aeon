;; wial (rd)

(define pls
  (lambda (clk div frq)
    (let* ((t (pulse-divider clk div 0))
           (e (decay2 t 0.05 0.75))
           (f (mul-add (toggle-ff t) frq (mul 2 frq))))
      (foldl1 mul (list (sin-osc ar f 0)
                        e
                        (ti-rand 0 1 t)
                        0.5)))))

(define plss
  (lambda (clk descr)
    (mix
     (make-mce
      (map (lambda (l)
             (let ((d (list-ref l 0))
                   (f (list-ref l 1))
                   (a (list-ref l 2)))
               (mul (pls clk d f) a)))
           descr)))))

(define smpl
 (lambda (f)
   (list (list (mce2 4 6)          f  0.75)
         (list (mce2 2 6) (mul f   2) 0.75)
         (list (mce2 1 2) (mul f  16) 0.025)
         (list (mce2 1 5) (mul f  64) 0.005)
         (list (mce2 1 3) (mul f 128) 0.035)
         (list (mce2 1 4) (mul f 256) 0.15)
         (list (mce2 2 3) (mul f 512) 0.35))))

(define wial
  (let ((clk (impulse ar 16 0)))
    (plss clk (smpl (tw-choose (dust kr 1)
                               (mce2 (* 20 2/3) 20)
                               (mce2 0.25 0.75)
                               0)))))

(hear wial)
