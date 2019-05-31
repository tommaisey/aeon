;; bytevector -> (port -> any) -> any
(define with-input-from-bytevector
  (lambda (b f)
    (let* ((p (open-bytevector-input-port b))
	   (r (f p)))
      (close-port p)
      r)))

;; bytevector -> int -> int -> bytevector
(define bytevector-section
  (lambda (v l r)
    (let* ((n (- r l))
	   (w (make-bytevector n 0)))
      (bytevector-copy! v l w 0 n)
      w)))

;; bytevector -> byte -> int
(define bytevector-find-index
  (lambda (v x)
    (letrec ((f (lambda (i)
		  (if (= (bytevector-u8-ref v i) x)
		      i
		      (f (+ i 1))))))
      (f 0))))

;; Tree bytevector -> bytevector
(define flatten-bytevectors
  (lambda (t)
    (let* ((l (flatten t))
	   (n (map bytevector-length l))
	   (m (sum n))
	   (v (make-bytevector m)))
      (let loop ((i 0)
		 (l l)
		 (n n))
	(if (null? l)
	    v
	    (let ((l0 (car l))
		  (n0 (car n)))
	      (bytevector-copy! l0 0 v i n0)
	      (loop (+ i n0) (cdr l) (cdr n))))))))

;; number a => (bytevector -> int -> a -> ()) -> int -> a
(define bytevector-make-and-set1
  (lambda (f k n)
    (let ((v (make-bytevector k 0)))
      (f v 0 n)
      v)))

;; number a => (bytevector -> int -> a -> ()) -> int -> a
(define bytevector-make-and-set
  (lambda (f k n)
    (let ((v (make-bytevector k 0)))
      (f v 0 n (endianness big))
      v)))
