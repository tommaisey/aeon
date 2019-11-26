;; (dyn-klank in freqScale freqOffset decayScale spec)

;; Dynklank is a bank of frequency resonators which can be used to
;; simulate the resonant modes of an object. Each mode is given a ring
;; time, which is the time for the mode to decay by 60 dB.

;; Unlike klank, the parameters in specificationsArrayRef can be
;; changed after it has been started.

(let ((i (mul (impulse ar 2 0) 0.1))
      (d (klank-data (list 800 1071 1153 1723)
		     (replicate 4 1)
		     (replicate 4 1))))
  (audition (out 0 (dyn-klank i 1 0 1 d))))

(let ((i (mul (dust ar 8) 0.1))
      (d (klank-data (list 800 1071 1353 1723)
		     (replicate 4 1)
		     (replicate 4 1))))
  (audition (out 0 (dyn-klank i 1 0 1 d))))

(let* ((i (mul (impulse ar 3 0) 0.1))
       (f (list 800 1071 1153 1723))
       (r (list 1 1 1 1))
       (x (mouse-x kr 0.5 2 1 0.1))
       (y (mouse-y kr 0.1 10 1 0.1))
       (d (klank-data (map (lambda (e) (mul e x)) f)
		      (replicate 4 1)
		      (map (lambda (e) (mul e y)) r))))
  (audition (out 0 (dyn-klank i 1 0 1 d))))

(let* ((i (lambda (f)
	    (mul (impulse ar (lin-lin (lf-noise0 kr f) -1 1 3 12) 0) 0.1)))
       (t (lambda (i d l r)
	    (map (lambda (e) (mul e (t-rand l r i))) d)))
       (d (lambda (i f r)
	    (klank-data (t i f 0.5 2)
			(replicate 4 1)
			(t i r 0.1 10))))
       (f1 (list 800 1071 1153 1723))
       (f2 (list 786 1083 1169 1715))
       (r1 (list 1 0.95 0.75 1.25))
       (r2 (list 1 1.35 0.95 1.15))
       (i1 (i 1.5))
       (i2 (i 1.25)))
  (audition (out 0 (mce2 (dyn-klank i1 1 0 1 (d i1 f1 r1))
			 (dyn-klank i2 1 0 1 (d i2 f2 r2))))))
