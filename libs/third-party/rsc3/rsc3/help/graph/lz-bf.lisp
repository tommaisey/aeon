;; lz-bf (rd)

(define lz-bf-i
  (let* ((x (mouse-x kr 1 12 0 0.1))
         (l (lorenz-l ar
                      sample-rate
                      (mul-add (lf-noise0 kr x) 2 12)
                      (mul-add (lf-noise0 kr x) 20 38)
                      (mul-add (lf-noise0 kr x) 1.5 3)
                      (mce2 0.025 0.05)
                      0.1 0.0 0.0))
         (p (phasor ar
                    0
                    (mul3 l 24 (buf-rate-scale kr 0))
                    0
                    (buf-frames kr 0) 0)))
    (buf-rd 1 ar 0 p 0 2)))

(define lz-bf
   (lambda (fn)
     (lambda (fd)
       (begin
         (async fd (b-alloc-read 0 fn 0 0))
         (play fd (out 0 lz-bf-i))))))

(with-sc3 (lz-bf "/home/rohan/data/audio/pf-c5.aif"))
