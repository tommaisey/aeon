(pattern example1
  (in! (over 1 4)
       (to: :sample hh
            :sample-idx (over 1 [24 26 35 48])
            :sustain 1/32
            :amp (over 1/4 [0.2 0.5])
            :pan (sine 1 0.2 0.8)
            :cutoff 1
            :resonance 0.5)
       (mv+ 1/8)
       (rp: (over 2 [[(taps 1/16 -6 (to* :cutoff 0.1)) (taps 1/32 -8 (to* :cutoff 0.2))]
                     (taps 1/16 -3 (to* :cutoff 0.6) (to: :resonance 0.1))
                     (taps 1/24 6 (to* :cutoff 0.1) (to: :resonance 0.8))]))
       main-swing))
