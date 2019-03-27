
(define bassln1
  (pdef [[I V] II III [V VII]]))
(define bassln2
  (pdef [I IV III II]))

(define p1
  (o->
    (in: :scd 1 [I II [V III]])
    (rp: 2/3 [! (pick [triad 11th 13th]) [7th 9th]])

    (in: :scd 4 [bassln1 bassln2] 
	 (:octave -1))

    (to: :cutoff [1 2 1.5 1])

    (in: :scd 2/3 bassln2 
	 (:octave [1 (pick [2 0]) 2])
	 (:cutoff [0.5 0.2 0.2 1]))

    (to: :scale dorian)
    (to: :inst "swirly-keys")
    (to: :attack 0.01)
    (to* :sustain 0.1)))
