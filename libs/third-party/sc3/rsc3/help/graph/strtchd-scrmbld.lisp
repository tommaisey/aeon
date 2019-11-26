;; strtchd-scrmbld (rd)

(define dust-r
  (lambda (r lo hi)
    (let ((d (dseq dinf (dwhite 1 lo hi))))
      (t-duty r d 0 0 (u:abs (white-noise r)) 1))))

(define rpr
  (lambda (n t)
    (let ((i (in 2 kr n)))
      (t-rand (mce-channel i 0) (mce-channel i 1) t))))

(define strtchd
  (lambda (b z)
    (let* ((clk (dust-r ar (in 1 kr 0) (in 1 kr 1)))
           (rat (rpr 2 clk))
           (dur (rpr 4 clk))
           (bdr (buf-dur kr b))
           (hbd (mul bdr 0.5))
           (pos (add (mul (rpr 8 clk) bdr)
                     (mul-add (lf-saw ar z 0) hbd hbd)))
           (pan (rpr 10 clk))
           (amp (rpr 6 clk)))
      (t-grains 2 clk b rat pos dur pan amp 2))))

(define scrmbld
  (lambda (u b t)
    (let* ((f (fft* b u))
           (g (pv-bin-scramble f
                               (mouse-x kr 0.5 1.0 0 0.1)
                               (mouse-y kr 0.5 1.0 0 0.1)
                               t)))
      (ifft* g))))

(define strtchd-scrmbld
  (let ((t0 (dust kr 0.01))
        (t1 (dust kr 0.02))
        (u (add (strtchd 10 0.015)
                (strtchd 10 0.0175))))
    (out 0 (mce2 (scrmbld (mce-channel u 0) 20 t0)
                 (scrmbld (mce-channel u 1) 30 t1)))))

(define mk-r-set
  (lambda ()
    (list (random 0.005 0.001)   (random 0.0075 0.0125)
          (random 0.90 0.975)    (random 1.025 1.10)
          (random 0.005 0.075)   (random 0.075 0.125)
          (random 0.005 0.01)    (random 0.15 0.25)
          (random -0.015 -0.005) (random 0.005 0.015)
          (random -1 0)          (random 0 1.0))))

(define pattern
  (lambda (fd)
    (begin
      (send fd (c-setn1 0 (mk-r-set)))
      (thread-sleep (choose (list 0.05 0.15 0.25 0.5 0.75 1.25)))
      (pattern fd))))

(with-sc3
 (lambda (fd)
   (begin
     (async fd (b-alloc-read 10 "/home/rohan/data/audio/pf-c5.snd" 0 0))
     (async fd (b-alloc 20 2048 1))
     (async fd (b-alloc 30 2048 1))
     (play fd strtchd-scrmbld)
     (pattern fd))))
