;; red frik (f0)

(define red
  (lambda (tr n)
    (let* ((r1 (t-rand 0.3 3 tr))
           (r2 (t-rand 0.3 5 tr))
           (r3 (t-rand 0 0.5 tr))
           (r4 (t-rand 0.49 0.56 tr))
           (r5 (t-rand 0.3 0.6 tr))
           (r6 (t-rand 0.3 0.5 tr))
           (o1 (mul-add (f-sin-osc kr r2 0) r3 r4))
           (o2 (mul (f-sin-osc kr o1 r5) r6)))
      (rhpf n r1 o2))))

(define red-frik
  (let ((n (clone 2 (brown-noise ar)))
        (tr (impulse kr 0.1 0)))
    (red tr n)))

(hear red-frik)
