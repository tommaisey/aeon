
(pattern wind1
  (in! 16
       (to: :sample wind
            :sample-idx (step 1/16 [3 10 8 1 5 9 6])
            :sample-pos (step 1/16 [0.13 0.43 0.23 0.75 0.83])
            :attack 0.002
            :sustain 1/16
            :release 1/8
            :bus2-amt (every 23 1/16 [0 0.4])
            :amp (sbdv 1/4 [0.2 0.5])
            :speed (pick [1/3 0.5 2/3 1 1.5]))
       (to+ :sample-pos (rnd 0.0 0.15))
       main-swing))