;; pattern buffer (rd)

(define tseq
  (lambda (l)
    (let ((n (/ (length l) 2)))
      (select (mul-add (lf-saw kr 0.5 0) n n) (make-mce l)))))

(define rs
  (lambda (nf fd)
    (let ((r0 (random 0 nf))
          (r1 (random 0 1)))
      (send fd (b-set1 10 r0 r1)))))

(define pattern-buffer
  (lambda (nf c)
    (let* ((p (phasor ar 0 (buf-rate-scale kr 10) 0 (buf-frames kr 10) 0))
           (t (buf-rd-c 1 ar 10 p 1))
           (r1 (replicate-m c (random 36 96)))
           (r2 (replicate-m c (random -1 1)))
           (r3 (i-random 0 2))
           (n1 (t-rand 0.02 0.08 t))
           (e (decay2 t 0.01 n1))
           (f (midi-cps (tseq r1)))
           (l (tseq r2))
           (o (list-ref (list (sin-osc ar f 0) (saw ar f)) r3)))
      (pan2 o l e))))

(with-sc3
 (lambda (fd)
   (let ((nf (* 2 48000))
         (c 24))
     (begin
       (async fd (b-alloc 10 (* nf 2) 1))
       (replicate-m c (rs nf fd))
       (play fd (out 0 (pattern-buffer nf c)))))))
