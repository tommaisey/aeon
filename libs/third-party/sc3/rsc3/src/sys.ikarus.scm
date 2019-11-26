;; double -> void
(define thread-sleep
  (lambda (p)
    (let* ((s (exact (floor p)))
	   (f (- p s))
	   (ns (exact (round (* f 1000000000)))))
      (ikarus:nanosleep s ns))))

;; void -> double
(define utcr
  (lambda ()
    (let* ((t (ikarus:current-time))
           (s (ikarus:time-second t))
           (n (ikarus:time-nanosecond t)))
      (+ s (/ n 1e9)))))

;; string -> io ()
(define system ikarus:system)
