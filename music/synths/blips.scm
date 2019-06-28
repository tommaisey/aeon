(pattern tune1
  (in! 16
       (to: :scd (step 1/4 [I V III (? [IV VI VII VIII])])
            :bus1-amt (sine 3 0.01 0.15)
            :amp 0.15
            :root Bb
            :pan (? 0.24 0.75)
            :attack 0.005
            :sustain (sine 8 1/32 1/3)
            :octave -1)
       (cp: (to: :octave (? [-2 0 1])))

       main-swing))