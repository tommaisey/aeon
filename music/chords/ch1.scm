(pattern ch1
  (in! (over 1 [1 [~ 1] [~ 1] [~ 1]])
       (to: :scd (over 4 [I III -3 -1])
            :scale minor
            :root Bb
            :octave -1
            :attack 1/32
            :release 1/8
            :bus1-amt 0.15
            :amp 0.25)
       (+->
         (to: :chd I
              :octave (over 1 [0 -1]))
         (to: :chd III)
         (to: :chd V)
         (to: :chd VII))
      main-swing))