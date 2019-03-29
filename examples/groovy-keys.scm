
(define bassln1
  (make-pdef-data [[I V] II III [V VII]]))
(define bassln2
  (make-pdef-data [I IV III II]))

(define p1
  (o->
    (in: :scd (/- 2 [I II [V III]])
	 (rp: (/- 2/3 [! (pick [triad 11th 13th]) [7th 9th]])))

    (in: :scd (/- 4 [bassln1 bassln2]) 
	 (to: :octave -1))

    (to: :cutoff (/- [1 2 1.5 1]))

    (in: :scd (/- 2/3 bassln2) 
	 (to: :octave (/- [1 (pick [2 0]) 2])
	      :cutoff (/- [0.5 0.2 0.2 1])))

    (to: :scale dorian
	 :inst "swirly-keys"
	 :attack 0.01)
    (to* :sustain 0.1)))
