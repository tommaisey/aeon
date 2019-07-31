(pattern chords1
  (o->
    (in! (over [[~ 1] [2 (? [1 2 4 6] [3 4 2 1])] 1 4]))
    (to: :amp 0.3
         :root Bb
         :octave -1
         :scd (step 2 [I IV -2 II]))

    (to* :sustain (? [1 3/2 2 3] [5 2 3 3] :chd))

    (to: :pan (? 0.4 0.6))

     main-swing

    (chord (over 1 [9th 7th]))))
