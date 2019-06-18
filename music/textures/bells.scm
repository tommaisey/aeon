(define rev-sample
  (x-> (to* :speed -1
            :sustain 2)
       (to: :sample-pos (rnd 0.1 0.2))))

(pattern bells
  (o->
    (in! (sbdv [1 [~ ~ 1 ~] [~ 1 ~ 1] 1])
         (to: :sample cy
              :sample-idx (sbdv [2 15 (rnd 10 14) (rnd 28 30)])
              :amp 1.5
              :bus1-amt 0.9
              :bus2-amt (sbdv 1/4 [(rnd 0.0 0.5) 0])
              :pan (rnd 0.25 0.75)
              :speed (pick [0.25 0.5 1 2])
              :attack 0.005
              :release 1/2)
         (to* :speed (rnd 0.98 1.01))
         (off rev-sample))

    (let ([tt 8])
      (in! tt
           (to: :sample cy
                :sample-idx (step (/ 1 tt) [23 (every 4 1/2 [29 12 15 18]) 14])
                :amp (step (/ 1 tt) [0.1 0.5 0.1 0.1 0.3])
                :pan (rnd 0.4 0.8)
                :bus1-amt 0.2
                :speed 1)
           (to* :sustain 0.75)))
    main-swing))