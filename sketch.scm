;; After loading init.scm, play around in this file.
;; Ctrl-C-C re-evaluates the definition you're in.

(start)
(pause)

;; A little beat
(define p1
  (let ([bd1 (in* (/- [4 2 [1 •] 1]))]
	[bd2 (in* (/- [1 2 [2 •] 1]))])
    (o->
      (in* (/- 2 [! bd2 bd1])
	   (to: :sample bd
		:amp (/- 1/2 [0.25 (rnd 0.01 0.1) (rnd 0.01 0.1) (rnd 0.01 0.1)])
		:speed 1))
     
      (in* (/- [1 4 (pick [• 2 4 3]) 1])
	   (to: :sample hh
		:amp (/- 1/2 [0.1 0.1 0.25 0.1])
		:speed (rnd 0.95 1.05)
		:pan (rnd 0.6 0.8)))
      
      (in* (/- [• 1 • 1])
	   (to: :sample sn
		:pan 0.45
		:speed 1))

      (in* (/- [•
		[(pick [3 2 4 •]) (pick [2 4 •])]
		(pick [• 1 3])
		[• (pick [• 1 2])]])
	   (to: :sample xt
		:amp (/- 1/2 [0.15 (rnd 0.1 0.4) (rnd 0.15 0.4) 0.15])
		:speed (rnd 0.8 0.85)
		:pan (rnd 0.2 0.8)))

      (to* :speed (/- [• 2]))
     
      (to: :inst "sampler"))))
