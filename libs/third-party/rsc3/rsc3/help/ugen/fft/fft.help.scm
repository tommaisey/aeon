;; (fft buffer in hop wintype active)
;; (fft* b i) => (fft b i 0.5 0 1)

(define g-01
  (let* ((s (mul (white-noise ar) 0.05))
         (c (fft* (local-buf 1 2048) s)))
    (ifft* c)))

(define g-02
  (let* ((f1 (squared (mul-add (sin-osc kr 0.08 0) 6 6.2)))
         (f2 (mul-add (sin-osc kr f1 0) 100 800))
         (s (sin-osc ar f2 0)))
    (ifft* (fft* (local-buf 1 2048) s))))
