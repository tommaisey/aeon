;; lg-timed (rd)

(define timed
  (lambda (r y p)
    (let* ((d0 (dser r p))
           (d1 (dcons 0 d0))
           (d2 (dser r y))
           (t (t-duty ar d1 0 remove-synth d2 1)))
      (latch t t))))

(define lg
  (lambda (u) (lag u 0.03)))

(define lg-timed
  (let* ((n (make-mce (list 52 76 66 67 68 69)))
         (a (make-mce (list 0.35 0.15 0.04 0.05 0.16 0.07)))
         (d (make-mce (list 0.1 0.5 0.09 0.08 0.07 0.3)))
         (x (mouse-x kr 0.5 1.25 linear 0.2))
         (tn (lg (timed dinf n (mul d x))))
         (ta (lg (timed dinf a (mul d x)))))
    (mul (sin-osc ar (midi-cps tn) 0) ta)))

(hear lg-timed)
