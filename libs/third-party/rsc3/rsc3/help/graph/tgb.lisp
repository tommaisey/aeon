;; tgb (rd)

(define mkls
  (lambda (bp t)
    (env-gen kr 1 1 0 1 remove-synth (env-bp-linear bp t 1))))

(define pm/t
  (lambda (l r d t)
    (let ((le (mkls l d))
          (re (mkls r d)))
      (t-rand le re t))))

(define wrp
  (lambda (i l r)
    (let ((m (fdiv (sub r l) 2)))
      (mul-add i m (add l m)))))

(define pm/n
  (lambda (rt l r d)
    (let ((le (mkls l d))
          (re (mkls r d)))
      (wrp (white-noise rt) le re))))

(define gb
  (lambda (b d)
    (let* ((gps (pm/n ar
                      (list 0 400 1 0900)
                      (list 0 600 1 1200)
                      d))
           (t (impulse ar gps 0))
           (dur (pm/t (list 0 0.005 0.5 0.015 1 0.005)
                      (list 0 0.009 0.5 0.020 1 0.009)
                      d
                      t))
           (pan (pm/t (list 0 -1.0 0.5 -0.5 1 0.5)
                      (list 0 -0.5 0.5 +0.5 1 1.0)
                      d
                      t))
           (rate (pm/t (list 0 06 0.5 12 1 06)
                       (list 0 12 0.5 12 1 12)
                       d
                       t))
           (cpos (pm/t (list 0 0 1 0.95)
                       (list 0 0 1 1.00)
                       d
                       t))
           (amp (pm/t (list 0 0.25 0.5 0.55 1.0 0.15)
                      (list 0 0.50 0.5 0.75 1.0 0.25)
                      d
                      t)))
      (t-grains 2 t b rate (mul cpos (buf-dur kr b)) dur pan amp 2))))

(define tgb
  (lambda (fn)
    (lambda (fd)
      (begin
        (async fd (b-alloc-read 10 fn 0 0))
        (play fd (out 0 (gb 10 7)))))))

(with-sc3 (tgb "/home/rohan/data/audio/pf-c5.snd"))

; longer, (hear (out 0 (gb 10 60)))
