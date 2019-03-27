

;; A groovy little number...

(define p1
  (o->
    ;; Keys
    (in: :scd 2 [[I III] I VI •]
	 (:octave [0 1 (pick [1 0]) 0]))

    (rp: 4 [! triad 7th 9th])

    (cp: (to: :octave -1)
	 (to+ :beat 1/12))

    (in: :scd 1 [I III IV]
	 (:octave 1))

    (in: :scd 2 [I [I III] IV II]
	 (:octave -1))

    (to: :scale dorian)
    (to: :inst "swirly-keys")
    (to: :attack 0.005)
    (to: :sustain 0)
    (to: :release [2 1 1 4])
    (to* :release 0.3)

    ;; Drums
    (in: :scd 1 [(× V 3) III [VI VIII] III]
	 (:attack 0.01)
	 (:sustain 0)
	 (:octave -1)
	 (:amp 0.1))

    (in: :sample 1/3 [• (× hh 3)]
	 (:amp 0.1)
	 (:pan (rnd 0.1 0.7)))
    (in* 2 [(rnd 1 3) 1 [1 1] 1]
      (:sample bd))
    (in: :sample 1 [• sn • sn]
	 (:amp 0.2))))
