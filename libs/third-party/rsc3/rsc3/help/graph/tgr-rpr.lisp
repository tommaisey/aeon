;; tgr-rpr (rd)

(define dust-r
  (lambda (r lo hi)
    (let ((d (dseq dinf (dwhite 1 lo hi))))
      (t-duty r d 0 0 (u:abs (white-noise r)) 1))))

(define rpr
  (lambda (n t)
    (let ((i (in 2 kr n)))
      (t-rand (mce-channel i 0) (mce-channel i 1) t))))

(define r-set
  (lambda (r)
    (if (> r 0.5)
        (list (random 0.005 0.025) (random 0.05 0.25)
              (random 0.75 0.95)   (random 1.05 1.25)
              (random 0.001 0.01)  (random 0.02 0.04)
              (random 0.1 0.2)     (random 0.2 0.4)
              (random 0.0 0.45)    (random 0.55 1.0)
              (random -1 0)        (random 0 1.0))
        (list (random 0.005 0.025) (random 0.05 0.25)
              (random -1.25 -1.05) (random -0.95 -0.75)
              (random 0.001 0.01)  (random 0.02 0.04)
              (random 0.1 0.2)     (random 0.2 0.4)
              (random 0.0 0.45)    (random 0.55 1.0)
              (random -1 0)        (random 0 1.0)))))

(define tgr-rpr
  (lambda (b)
    (let* ((clk (dust-r ar (in 1 kr 0) (in 1 kr 1)))
           (rat (rpr 2 clk))
           (dur (rpr 4 clk))
           (pos (mul (rpr 8 clk) (buf-dur kr b)))
           (pan (rpr 10 clk))
           (amp (rpr 6 clk)))
      (t-grains 2 clk b rat pos dur pan amp 2))))

(define pattern
  (lambda (fd)
    (begin
      (send fd (c-setn1 0 (r-set (random 0 1))))
      (thread-sleep (choose (list 0.25 0.75 1.5)))
      (pattern fd))))

(with-sc3
 (lambda (fd)
   (begin
     (async fd (b-alloc-read 10 "/home/rohan/data/audio/pf-c5.snd" 0 0))
     (play fd (out 0 (tgr-rpr 10)))
     (pattern fd))))
