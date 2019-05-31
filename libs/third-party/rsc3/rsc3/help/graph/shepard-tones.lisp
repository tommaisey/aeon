;; shepard tones (alberto de campo)

(define iota
  (lambda (n l j)
    (if (= n 0)
        nil
        (cons l (iota (- n 1) (+ l j) j)))))

(define indxs
  (lambda (n l r)
    (iota n l (/ (- r l) n))))

(define hanning-window
  (lambda (n)
    (let* ((lp (* pi -0.5))
           (rp (+ lp (* 2 pi)))
           (hf (lambda (i) (+ (* (sin i) 0.5) 0.5))))
      (map hf (indxs n lp rp)))))

(define square (lambda (x) (* x x)))

(define amp-table (map square (hanning-window 1024)))

(define amp-f (lambda (i) (* (expt 0.5 i) 20000)))

(define freq-table (map amp-f (indxs 1024 0 10)))

(define shepard-tones
  (let* ((rate-scale (/ (/ 1024 44100) 10))
         (rate 0.1)
         (ph (phasor ar 0 (* rate rate-scale) 0 1024 0))
         (phases (make-mce
                  (map
                   (lambda (n) (add (mul3 n 0.1 1024) ph))
                   (enum-from-to 0 9))))
         (freqs (buf-rd-c 1 ar 1 phases 1))
         (amps (buf-rd-c 1 ar 2 phases 1)))
    (mul (mix (mul (sin-osc ar freqs 0) amps)) 0.1)))

(with-sc3
 (lambda (fd)
   (begin
     (async fd (b-alloc 1 1024 1))
     (async fd (b-alloc 2 1024 1))
     (send fd (b-setn1 1 0 freq-table))
     (send fd (b-setn1 2 0 amp-table))
     (hear shepard-tones))))
