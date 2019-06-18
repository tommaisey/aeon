(pattern bd2
  (in! (step 1/4 [1 [1 » » »] [1 » » »] 1 
                  1 [1 1 » »] [1 » (pick [~ 1]) 1] 1])
       (to: :sample (bd/ 80)
            :amp 0.25
            :speed 0.84)
       main-swing))