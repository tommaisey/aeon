(let ((d (klang-data '(440 550 660 770 880 990 1000)
		     '(0.05 0.02 0.07 0.04 0.05 0.02 0.03)
		     (replicate 7 0))))
  (audition (out 0 (klang ar 1 0 d))))
