;; scheme functions for scsynth operators and ugens

;; ord a => a -> a -> a
(define s:lt
  (lambda (p q)
    (if (< p q) 1 0)))

;; ord a => a -> a -> a
(define s:le
  (lambda (p q)
    (if (<= p q) 1 0)))

;; ord a => a -> a -> a
(define s:ge
  (lambda (p q)
    (if (>= p q) 1 0)))

;; ord a => a -> a -> a
(define s:gt
  (lambda (p q)
    (if (> p q) 1 0)))

;; real -> real -> real
(define s:round
  (lambda (p q)
    (* (round (/ p q)) q)))

;; ord a => a -> a -> a -> a
(define s:clip
  (lambda (a b n)
    (cond ((< n a) a)
	  ((> n b) b)
	  (else n))))

;; number a => a -> a
(define s:squared
  (lambda (n)
    (* n n)))

;; number a => a -> a
(define s:cubed
  (lambda (n)
    (* n n n)))

;; number a => a -> a
(define s:recip
  (lambda (n)
    (/ 1 n)))

;; float -> float
(define s:log2
  (lambda (x)
    (/ (log (abs x)) (log 2))))

;; float -> float
(define s:log10
  (lambda (x)
    (/ (log x) (log 10))))

;; float -> float
(define s:amp-db
  (lambda (x)
    (* (s:log10 x) 20)))

;; float -> float
(define s:db-amp
  (lambda (x)
    (expt 10 (* x 0.05))))

;; float -> float
(define s:pow-db
  (lambda (x)
    (* (s:log10 x) 10)))

;; float -> float
(define s:db-pow
  (lambda (x)
    (expt 10 (* x 0.1))))

;; float -> float
(define s:midi-cps
  (lambda (note)
    (* 440.0 (expt 2.0 (* (- note 69.0) (/ 1.0 12.0))))))

;; float -> float
(define s:cps-midi
  (lambda (freq)
    (+ (* (s:log2 (* freq (/ 1.0 440.0))) 12.0) 69.0)))

;; float -> float
(define s:midi-ratio
  (lambda (midi)
    (expt 2.0 (* midi (/ 1.0 12.0)))))

;; float -> float
(define s:ratio-midi
  (lambda (ratio)
    (* 12.0 (s:log2 ratio))))

;; float -> float
(define s:oct-cps
  (lambda (note)
    (* 440.0 (expt 2.0 (- note 4.75)))))

;; float -> float
(define s:cps-oct
  (lambda (freq)
    (+ (s:log2 (* freq (/ 1.0 440.0))) 4.75)))

;; float -> [float] -> int -> float
(define s:degree-to-key
  (lambda (degree scale steps)
    (let ((scale-n (length scale)))
      (+ (* steps (div degree scale-n))
	 (list-ref scale (exact (mod degree scale-n)))))))

;; (s:l-choose (list 1 3 5 7 9))
(define s:l-choose
  (lambda (l)
    (list-ref l (i-random 0 (length l)))))
