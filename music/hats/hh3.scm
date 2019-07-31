(pattern hh3
  (in! 16
       (to: :sample hh
            :sample-idx (over 1/8 [30 12])
            :amp (over 1/4 [0.05 0.1])
            :pan (over 1/4 [0.3 0.7])
            :sustain (every 5 1/16 [1/64 1/6]))
       main-swing))
