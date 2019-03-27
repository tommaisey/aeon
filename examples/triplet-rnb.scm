

;; Groovy little number now...?
(define p1
  (o->
    (in: :scd 2 [[I III] I VI •])

    (rp: 4 [! triad 7th 9th])

    (cp: (to: :octave -1)
	 (to+ :beat 1/12))
    
    (to: :scale dorian)
    
    (in: :scd 1 [(× V 3) III [VI VIII] III]
	 (:attack 0.01)
	 (:sustain 0)
	 (:octave -1)
	 (:amp 0.1))

    (in: :sample 1/3 [• (× hh 3)]
	 (:amp 0.15)
	 (:pan (rnd 0.1 0.7)))
    (in* 2 [(rnd 1 3) 1 [1 1] 1]
      (:sample bd))
    (in: :sample 1 [• sn • sn]
	 (:amp 0.2))))
