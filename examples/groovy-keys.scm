
(define bassln1
  (make-pdef-data [[I V] II III [V VII]]))
(define bassln2
  (make-pdef-data [I IV III II]))

(pattern p1
  (o->
    (in: :scd (sbdv 2 [I II [V III]])
         (rp: (sbdv 2/3 [~ (? [(chord triad) (chord 11th) (chord 13th)]) 
                           [(chord 7th) (chord 9th)]])))

    (in: :scd (sbdv 4 [bassln1 bassln2])
         (to: :octave -1))

    (to: :cutoff (sbdv [1 2 1.5 1]))

    (in: :scd (sbdv 2/3 bassln2)
         (to: :octave (sbdv [1 (? [2 0]) 2])
              :cutoff (sbdv [0.5 0.2 0.2 1])))

    (to: :scale dorian
         :inst "swirly-keys"
         :attack 0.01)
    (to* :sustain 0.1)))
