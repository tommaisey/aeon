;; (running-sum in numsamp)

;; A running sum over a user specified number of samples, useful for
;; running RMS power windowing.

;; in      - input signal
;; numsamp - How many samples to take the running sum over 
;;           (initialisation rate)

(let ((n 40))
  (audition
   (out 0 (foldl1 mul (list (sin-osc ar 440 0)
			    (running-sum (sound-in (mce2 0 1)) n)
			    (recip n))))))
