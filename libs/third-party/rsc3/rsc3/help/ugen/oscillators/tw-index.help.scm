;; Assuming normalized values

(hear
 (let* ((prob (mce3 1/5 2/5 2/5))
	(freq (mce3 400 500 600))
	(f (select (tw-index (impulse kr 6 0) 0.0 prob) freq)))
   (mul (sin-osc ar f 0) 0.2)))

;; Modulating probability values

(hear
 (let* ((t (impulse kr 6 0))
	(a (mce3 1/4 1/2 (mul-add (sin-osc kr 0.3 0) 0.5 0.5)))
	(f (select (tw-index t 1.0 a) (mce3 400 500 600))))
   (mul (sin-osc ar f 0) 0.2)))
