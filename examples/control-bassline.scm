

(define grp1 (make-unused-group-id))

(define (cutoff-control key grp pdef res offset)
  (in: key pdef
       (to: :resonance res
	    :group grp :control "cutoff")
       (mv+ offset)))

(define p1
  (o->
      (in* (/- 2 1)
	   (to: :group grp1
		:attack 0.01
		:release 0.1
		:amp 0.3
		:inst "dual-lopass"))

      (in: :chd (/- 1/2 [I I (pick [I III]) I])
	   (to+ :chd (/- 4 [I I I V]))
	   (to: :scd (/- 2 [I (each 2 [VI III])])
		:octave (/- 2 [-1 (pick [0 1]) (pick [0 1 -1])])
		:bus1-amt (rnd 0.0 0.125)
		:pan (/- 1/2 [0.4 0.6])
		:group grp1 :control "pitchbend"))

      (cutoff-control :cutoff1 grp1
		      (/- 1/4 (rnd 4.0 12.0))
		      (rnd 0.3 1.8) 0)
      (cutoff-control :cutoff2 grp1
		      (/- 1/8 (rnd 1.0 3.0))
		      (rnd 0.1 0.8)
		      (/- 1/4 (pick [0 0 1/16 1/8])))

      (to: :root -5
	   :scale minor)))
