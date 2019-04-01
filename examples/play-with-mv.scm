
(define p1
  (o->
    (in: :scd (/- 1/4 I)
	 (to: :octave -1)
	 (to* :sustain (/- [0.9 0.8 3 0.9])))
    
    (in: :scd (/- [II V I I])
	 (mv+ (/- [0 1/8 0 1/16]))
	 (mv- (/- [• 1/16 1/16 •]))	 
	 (+-> (mv+ (/- 0))		
	      (x-> (mv+ (/- 1/16))		
		   (to+ :scd (/- 3/2 [0 -2 2 8 -6]))) 
	      (x-> (mv+ (/- 1/8))		
		   (to: :octave (/- 3/2 [0 1])))
	      (x-> (mv+ (/- 1/4))			
		   (to+ :scd (pick [I V IV])))))

    (to: :scale major)
    (to* :sustain (/- 1 [1 1.75 1]))
    (mv+ 2/4)))
