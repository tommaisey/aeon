(pattern hh2
  (in! 24
       (to: :sample hh
            :sample-idx (step 1/16 [30 12 46])
            :amp (over 1/4 [0.05 0.1])
            :pan (over 1/4 [0.3 0.7])
            :sustain 1/64)
       (mv* 3/2)
       main-swing))