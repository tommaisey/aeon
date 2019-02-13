;; After loading init.scm, play around in this file.
;; Ctrl-C-C re-evaluates the definition you're in.

(start)
(pause)

(define p1
  (in+ 1 [4 2 3 1]))

(define p1
  (o->
   (in+ 1 [2 (pick [~ 1 3]) (pick [1 3 ~]) 2])
   (to! :amp 1/2 [0.3 0.05 (rnd 0.1 0.15)])
   (cp! (to! :freq [660 550])
	(to! :sustain (pick [0.2 0.4])))))

(define p1
  (o->

   ;; Shorthand for similar form as above
   (in+ 1 [2 (pick [~ 1 3]) (pick [1 3 ~]) 2]
     (:sample bd)
     (:inst "sampler-mono")
     (:amp 1/2 [0.3 0.05 (rnd 0.1 0.15)]))

   (in+ 1 [~ 1 ~ 1]
     (:sample sn)
     (:inst "sampler-mono")
     (:amp 0.4)
     (:sustain 0.1)
     (:release 0.1))

   (in+ 1/4 [1 (rnd 0 1) 1 (rnd 0 3)]
     (:sample hh)
     (:inst "sampler-mono")
     (:amp 1/3 [0.1 0.2])
     (:pan (rnd 0.0 1.0)))

   (in+ 2/3 [1 (pick [~ 1 2]) (pick [~ 1 2]) 1]
     (:inst "sine-grain")
     (:freq 12/3 [440 (pick [(/ 440 3/2) (/ 440 5/4)])
		      (pick [(* 220 3/2) (* 440 1/2)])])
     (:amp 2/3 [0.35 0.2 0.2])
     (:sustain 2/3 [1 1.5 1 0.75]))

   (in+ 1 [(rnd 3 9) 4]
     (:inst "sine-grain")
     (:freq 3 [880 660 [440 990]])
     (:freq 4 [~ ~ [(pick [1110 550]) [(pick [1320 660]) 1760]]])
     (:pan 1/2 [0.25 0.75]))))


(define p1
  (o->
   (in+ 2 [[2 (rnd 1 5) 2 4] [(rnd 2 6) 3 1 ~]]
     (:freq 1   [220 330 110])
     (:length 3 [0.8 0.4 1.2 0.3]))
   
   (in+ 1 [~ (pick [3 6 8]) 2 1]
     (:freq 3   [(* 110 3) 880 990 (* 110 6)])
     (:freq 3/2 [~ [~ 440]]))

   (in+ 1/2 [~ 1]
     (:inst "sampler-mono")
     (:sample sn))

   (in+ 1 [1 ~ [(rnd 0 1) ~ ~ (rnd 0 4)] (rnd 0 2)]
     (:inst "sampler-mono")
     (:sample bd))

   (in+ 1/2 [~ (rnd 1 6) ~ 1]
     (:inst "sampler-mono")
     (:sample hh))))
