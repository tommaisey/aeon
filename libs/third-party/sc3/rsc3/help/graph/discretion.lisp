;; discretion (rd)

(define discretion
  (let* ((mkls (lambda (bp t)
                 (let ((c (replicate (/ (length bp) 2) 1)))
                   (env-gen kr 1 1 0 1 remove-synth (env-bp bp t 1 c)))))
         (part (lambda (_)
                 (let* ((f1 (clone 2 (rand 50 55)))
                        (f2 (clone 2 (rand 50 65)))
                        (f3 (clone 2 (rand 50 55)))
                        (a (clone 2 (rand 0.01 0.035)))
                        (t 21)
                        (f (mkls (list 0.0 f1 0.33 f2 1.0 f3) t))
                        (g (mkls (list 0 0 0.33 a 1 0) t)))
                   (mul (saw ar f) g)))))
    (mix-fill 8 part)))

(hear discretion)
