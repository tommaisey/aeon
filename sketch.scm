;; After loading init.scm, play around in this file.
;; Ctrl-C-C re-evaluates the definition you're in.
(start)
(pause)

(define p1
  (o->
    (in: :scd (/+ 1/16 [I II III V » V VIII VII])
	 (to: :chd (/+ 2 [I IV I II])
	      :pan (sine 3/2 0.3 0.7)))
    (in: :scd (/- [I I I I])
	 (to: :octave -1
	      :chd (/- 4 [I IV VI III])))
    (to: :inst "swirly-keys"
	 :sustain 0.0
	 :attack 0.001
	 :release (sine 4 0.05 0.5)
	 :bus2-amt (sine 3 0.05 0.15))

    (o->
      (in: :sample (/- [bd [~ bd ~ ~] (pick [[bd ~ bd bd] [~ bd bd ~]]) bd]))
      (in: :sample (/- [~ (pick [sn [sn ~ ~ sn]]) ~ sn])
	   (to* :sustain 0)
	   (to* :release (rnd 0.8 2.0)))
      (in: :sample (/+ 1/16 [hh hh ~ hh ~])
	   (to: :amp 0.075))
      (in: :sample (/+ 1/16 [~ ~ ~ ~ oh ~ ~ ~ ~])
	   (to: :amp 0.1)))))

(define p1
  (o->
    (in: :sample (/- [bd [~ bd bd ~] [~ bd ~ bd] [~ ~ ~ bd]]))
    (in: :sample (/- [~ sn ~ sn])
	 (to: :amp 0.15))

    (in: :amp (/- 1/8 [0.05 0.1])
	 (to: :sample hh))
    (in: :amp (/- 1/4 [~ 0.24])
	 (to: :sample oh
	      :pan 0.7
	      :amp 0.175))))

;; A little beat
(define p1
  (let ([bd1 (in* (/- [4 2 [1 ~] 1]))]
	[bd2 (in* (/- [1 2 [2 ~] 1]))])
    (o->
      (in* (/- 2 [! bd2 bd1])
	   (to: :sample bd
		:amp (/- 1/2 [0.25 (rnd 0.01 0.1) » »])
		:speed 1))
     
      (in* (/- [1 4 (pick [~ 2 4 3]) 1])
	   (to: :sample hh
		:amp (/- 1/2 [0.1 0.1 0.25 0.1])
		:speed (rnd 0.95 1.05)
		:pan (rnd 0.6 0.8)
		:bus1-amt (rnd 0.001 0.015)))
      
      (in* (/- [~ 1 ~ 1])
	   (to: :sample sn
		:pan 0.45
		:speed 1
		:bus1-amt 0.02))

      (in* (/- [~
		[(pick [3 2 4 ~]) (pick [2 4 ~])]
		(pick [~ 1 3])
		[~ (pick [~ 1 2])]])
	   (to: :sample xt
		:amp (/- 1/2 [0.15 (rnd 0.1 0.4) (rnd 0.15 0.4) 0.15])
		:speed (rnd 0.8 0.85)
		:pan (rnd 0.2 0.8)
		:bus1-amt (rnd 0.0 0.05)))

      (to* :speed (/- [~ 2]))
     
      (to: :inst "sampler"))))
