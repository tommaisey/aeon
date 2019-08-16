(pattern canon1
  (in! (over 1 [1 [1 1] 1 [1 1 1]])

       (to: :scale-degree (over 1 [I V III IV])
            :bus1-amt (sine 3 0.0 0.3)
            :root Bb
            :scale minor
            :amp 0.2)

       (+->
         (x->)
         (x-> (mv* 1/2)
              (to: :chd (over 4 [-7 -4 -6 -9])))
         (x-> (mv* 3/2)
              (to: :chd (over [-2 2 -2 -3]))))

       (to: :pan (? 0.2 0.8))
        main-swing))