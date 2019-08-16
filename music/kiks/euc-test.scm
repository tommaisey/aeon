(pattern euc-test
  (o->
    (in! (over 1 [(euc 8 3 0) (euc 8 6 1)])
         (to: :amp 0.6
              :sample cy
              :sample-idx (over 6 [(? 1 5) (? 15 18)])
              :bus1-amt 0.1
              :speed (every 4 1/4 [3/4 1/2])))

    (in! (over 1/2 (euc 8 5 3))
         (to: :amp (over 1/2 [0.18 0.15 [0.15 0.1] [0.18 0.125]])
              :sample hh
              :sample-idx (? [3 28])
              :bus1-amt 0.02))

    (in! (over 4 (map (lambda (hits) (euc 16 hits 0)) (list 4 9 7 6)))
         (to: :amp (over 1/2 [0.15 0.18 [0.15 0.1] [0.18 0.125]])
              :sample bd
              :sample-idx 7
              :bus1-amt 0.01))

    (in! (over 4 [(euc 16 2 4) × × (euc 16 7 7)])
         (to: :amp 0.1
              :sample sn
              :sample-idx 7
              :bus1-amt 0.01))
    main-swing))

(start)
(pause)