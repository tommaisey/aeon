(define rev-sample
  (x-> (to* :speed -1
            :sustain 2)
       (to: :sample-pos (? 0.1 0.2))))

(pattern bells
  (o->
    (in! (over [1 [~ ~ 1 ~] [~ 1 ~ 1] 1])
         (to: :sample cy
              :sample-idx (over [2 15 (? 10 14) (? 28 30)])
              :amp 1.5
              :bus1-amt 0.9
              :bus2-amt (over 1/4 [(? 0.0 0.5) 0])
              :pan (? 0.25 0.75)
              :speed (? [0.25 0.5 1 2])
              :attack 0.005
              :release 1/2)
         (to* :speed (rnd 0.98 1.01))
         (off rev-sample))

    (let ([tt 8])
      (in! tt
           (to: :sample cy
                :sample-idx (step (/ 1 tt) [23 (every 4 1/2 [29 12 15 18]) 14])
                :amp (step (/ 1 tt) [0.1 0.5 0.1 0.1 0.3])
                :pan (? 0.4 0.8)
                :bus1-amt 0.2
                :speed 1)
           (to* :sustain 0.75)))
    main-swing))
