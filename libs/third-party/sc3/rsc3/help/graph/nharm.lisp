;; nharm (rd)

(define nharm
  (lambda (n f)
    (if (<= n 0)
        (list)
        (cons f (nharm (- n 1) (add f f))))))

(define klg
  (lambda (m u)
    (let* ((n (i-random 4 u))
           (d (i-random 9 12))
           (a 0.5)
           (e (env-gen kr 1 0.9 0 1 remove-synth (env-sine d a)))
           (s (klang-data (nharm n (midi-cps (random m (+ m 2))))
                          (replicate-m n (random 0.01 0.02))
                          (replicate n 0))))
      (pan2 (klang ar 1 0 s)
            (random -1 1)
            e))))

(define pattern
  (lambda (fd)
    (begin
      (play fd (out 0 (klg (random 32 92)
                           (i-random 9 24))))
      (thread-sleep (random 0.25 0.75))
      (pattern fd))))

(with-sc3 pattern)
