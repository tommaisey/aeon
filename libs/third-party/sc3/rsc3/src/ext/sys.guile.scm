; void -> double
(define utcr
  (lambda ()
    (let* ((t (gettimeofday))
           (s (car t))
           (u (cdr t)))
      (+ s (/ u 1e6)))))

; double -> void
(define thread-sleep
  (lambda (p)
    (let ((u (inexact->exact (round (* p 1e6)))))
      (usleep u))))

; string -> io ()
; (define system system)
