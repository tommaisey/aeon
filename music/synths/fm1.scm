(pattern fm
  (in! (over 1 [1 [1 ~] [~ 1] [~ 1]])

       (to: :inst "fm-grain" 
            :fm-amt (? 0.5 2)
            :ratio (over 1 [7 3 9 3 12])
            :pan (? 0.3 0.7)
            :bus1-amt (sine 8 0.0 0.5)
            :root Bb
            :octave -1
            :amp 0.4)

       (+-> (x->)
            (x-> (mv* 1/2)
                 (to: :chd (over 6 [VI II V III])
                      :sustain 1/16)
                 (taps 1/8 2 (to+ :chd 2 :amp -0.1) nowt))

            (x-> (mv* 3/2)
                 (to: :octave -2
                      :chd (over 8 [I  [IV III » » »] 
                                    II [VII IV » » »]])
                      :bus1-amt (? 0.1 0.4)
                      :fm-amt (? 0.1 0.4))
                 (to* :sustain 2)))

       main-swing))

(start)