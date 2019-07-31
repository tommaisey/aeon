(define fill1 (pdef [~ ~ [~ ~ 1 ~] [~ ~ ~ 1]]))
(define fill2 (pdef [~ ~ [~ ~ 1 1] [1 ~ 1 1]]))
(define fill3 (pdef [1 1 [~ 1 ~ 1] [1 ~ ~ 1]]))

(pattern sn1
  (o->
    (in! (over [~ 1 ~ 1])
         (to: :sample (sn/ 78)
              :amp 0.4
              :bus1-amt 0.03))

    (in! (every 4 1 [~ fill1 fill2 fill3])
         (to: :sample (sn/ 28)
              :amp 0.3
              :bus1-amt 0.04))
    main-swing))
