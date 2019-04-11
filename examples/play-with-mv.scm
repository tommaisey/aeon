
(define p1
  (o->
    ;; Steady bass pulse
    (in: :scd (/- 1/4 I)
	 (to: :octave -1)
	 (to* :sustain (/- [0.75 0.5 0.5 0.5])))

    ;; Simply melody shifted repeatedly
    (in: :scd (/- 2 [[II II] [V V] [I [I X I XII]] [I I]])
	 (to: :amp (/- [0.4 0.5])
	      :attack 0.0025)
	 (mv+ (/- [0 1/8 0 1/16]))
	 (mv- (/- [~ 1/16 1/16 ~]))	 
	 (+-> (mv+ (/- 0))		
	      (x-> (mv+ (/- 1/16))		
		   (to: :chd (/- 4 [0 -2 2 8 -6]))) 
	      (x-> (mv+ (/- 1/8))		
		   (to: :octave (/- 3/2 [0 1])
			:bus1-amt (rnd 0.0 0.3)
			:pan (sine 2 0.6 0.8)))
	      (x-> (mv+ (/- 1/4))			
		   (to+ :chd (each 1 [I V IV]))
		   (to: :bus1-amt (rnd 0.0 0.2))
		   (to: :pan (sine 2 0.1 0.2))))
	 (to* :sustain (/- 1 [(rnd 0.75 1.0) (rnd 0.3 0.5) 0.25])))

    (to* :bus1-amt (/- [1.0 (rnd 1.0 2.0)]))
    (to: :scale major)
    (mv+ 2/4)))
