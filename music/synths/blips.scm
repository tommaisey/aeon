(pattern tune1
  (in! 16
       (to: :scd (step 1/4 [I V III (pick [IV VI VII VIII])])
            :bus1-amt (sine 3 0.01 0.15)
            :amp 0.15
            :root Bb
            :pan (rnd 0.24 0.75)
            :attack 0.005)
       (cp: (to: :octave (pick [-1 1])))
       main-swing))