
(pattern p1 
  (part
   ;; Steady bass pulse
   (in: :scd (sbdv 1/4 I)
        (to: :octave -1
             :amp 0.3)
        (to* :sustain (sbdv [0.75 0.5 0.5 0.5]))
        (mv+ 1/8))

   ;; Simply melody shifted repeatedly
   (in: :scd (sbdv 2 [[II II] [V V] [I [I X I XII]] [I I]])
        (to: :amp (sbdv [0.4 0.5])
             :attack 0.0025)
        (mv+ (sbdv [0 1/8 0 1/16]))
        (mv- (sbdv [~ 1/16 1/16 ~]))
        (copy (mv+ (sbdv 0))
              (with (mv+ (sbdv 1/16))
                    (to: :chd (sbdv 4 [0 -2 2 8 -6])))
              (with (mv+ (sbdv 1/8))
                    (to: :octave (sbdv 3/2 [0 1])
                         :bus1-amt (? 0.0 0.3)
                         :pan (sine 2 0.6 0.8)))
              (with (mv+ (sbdv 1/4))
                    (to+ :chd (each 1 [I V IV]))
                    (to: :bus1-amt (? 0.0 0.2))
                    (to: :pan (sine 2 0.1 0.2))))
        (to* :sustain (sbdv 1 [(? 0.75 1.0) (? 0.3 0.5) 0.25])))

   (to* :bus1-amt (sbdv [1.0 (? 1.0 2.0)]))
   (to: :scale major)
   (mv+ 2/4)))
