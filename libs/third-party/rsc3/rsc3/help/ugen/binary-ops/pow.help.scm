;; (pow a b)

;; Exponentiation, written ** in sclang.  When the signal is negative
;; this function extends the usual definition of exponentiation and
;; returns neg(neg(a) ** b). This allows exponentiation of negative
;; signal values by noninteger exponents.

(audition
 (out 0 (let ((a (mul (f-sin-osc ar 100 0) 0.1)))
	  (mce2 a (pow a 10)))))

;; http://create.ucsb.edu/pipermail/sc-users/2006-December/029998.html

(let* ((n0 (mul-add (lf-noise2 kr 8) 200 300))
       (n1 (mul-add (lf-noise2 kr 3) 10 20))
       (s  (blip ar n0 n1))
       (x  (mouse-x kr 1000 (mul sample-rate 0.5) 1 0.1))
       (y  (mouse-y kr 1 24 1 0.1))
       (d  (latch s (impulse ar x 0)))
       (b  (u:round d (pow 0.5 y))))
  (audition
   (out 0 (mce2 d b))))
