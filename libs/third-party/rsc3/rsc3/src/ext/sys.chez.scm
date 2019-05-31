;; double -> void
(define thread-sleep
  (lambda (secs)
    (sleep (make-time 'time-duration 0 secs))))

;; void -> double
(define utcr
  (lambda ()
    (let* ((t (current-time))
	   (s (time-second t))
	   (n (time-nanosecond t)))
      (+ s (/ n 1e9)))))
