(pattern canon2
  (in! (over 1 [1 [1 1] 1 [~ [1 1 1]]])
       (to: :scale-degree (over 2 [I V III IV])
            :pan (over [0.2 0.5 0.7 0.9])
            :amp 0.2
            :octave -1
            :root Bb
            :bus1-amt 0.2
            ; :bus2-amt (pick [0 0 0 0 0.1 0.15])
            :inst "swirly-keys"
            :attack 0.001
            :release 1/16
            :sustain 0)
       (+->
         (x->)
         (x-> (mv* 3/2)
              (to: :octave 0
                   :chord-degree -3))
         (x-> (mv* 2)
              (to: :chord-degree -7)))
       main-swing))