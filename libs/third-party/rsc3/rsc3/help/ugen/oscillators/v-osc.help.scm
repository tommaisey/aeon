;; Allocate and fill tables 0 to 7 [see also Vosc3]

(with-sc3
 (lambda (fd)
   (let* ((square
	   (lambda (a) (* a a)))
	  (nth
	   (lambda (i)
	     (async fd (b-alloc i 1024 1))
	     (let* ((n (expt (+ i 1) 2))
		    (a (map1 (lambda (j) (square (/ (- n j) n)))
			     (enum-from-to 0 (- n 1)))))
	       (async fd (b-gen1 i "sine1" (cons 7 a)))))))
     (for-each nth (enum-from-to 0 7)))))

;; oscillator at buffers 0 through 7.

(let ((b (mouse-x kr 0 7 0 0.1))
      (f (mce2 120 121)))
  (audition (out 0 (mul (v-osc ar b f 0) 0.3))))

;; Reallocate buffers while oscillator is running.

(with-sc3
 (lambda (fd)
   (for-each
    (lambda (i)
      (async fd (b-gen1  i "sine1" (cons 7 (replicate-m 16 (random 0 1))))))
    (enum-from-to 0 7))))
