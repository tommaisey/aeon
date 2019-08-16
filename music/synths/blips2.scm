(pattern blips2
  (in! (over 1/2 [1 3 [~ 1] 2])

       (to: :scd (over [I IV VI (? [II V III])])
            :root Bb
            :octave -1
            :bus1-amt 0.0
            :amp 0.3)

       (taps 1/16 (over 4 [(? [-3 -4]) -2])
             (x->
               (to+ :bus1-amt 0.075)
               (to* :amp 0.8))
             (x->
               (to+ :scd (over [2 4 5]))))

       (to: :amp (? 0.05 (over [0.2 0.5 0.2 0.4])))

       main-swing))