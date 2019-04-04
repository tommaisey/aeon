

(define grp1 (make-unused-group-id))
(define grp2 (make-unused-group-id))

(define (cutoff-control key grp pdef res offset)
  (in: key (/- 1/4 (rnd 2.0 16.0))
       (to: :resonance res
	    :group grp :control "cutoff")
       (mv+ offset)))

(define p1
  (let ([scd (/- 4 [I V I [VIII V]])])
    (o->
      (in: :scd scd
	   (to: :group (/- 6 [grp1 » »]))
	   (to: :attack 0.01)
	   (to: :release 0.1)
	   (to: :inst "dual-lopass"))

      (in: :chd (/- 1/2 [[I II] (each 1 [I I IV]) (each 1/2 [II VIII]) •])
	   (to+ :chd (/- 4 [I I I IV]))
	   (to: :scd scd
		:octave (/- 2 [-1 (pick [0 1]) (pick [0 1 -1])])
		:pan (/- 1/2 [0.4 0.6])
		:group grp1 :control "pitchbend"))

      (in: :chd (/- 1/4 [II I (each 4 [V VIII]) •])
	   (to: :scd scd
		:octave -1
		:root (/- 4 [I III I IV])
		:pan (/- 1/4 (rnd 0.4 0.6))
		:group grp2 :control "pitchbend"))

      (cutoff-control :cutoff1 grp1 (/- 1/4 (rnd 2.0 16.0)) (rnd 0.3 1.5) 0)
      (cutoff-control :cutoff1 grp2 (/- 1/8 (rnd 0.5 10.0)) (rnd 0.1 0.8) 0)
      (cutoff-control :cutoff2 grp1 (/- 1/8 (rnd 0.7 7.0))  (rnd 0.1 0.8) (/- 1/4 (pick [0 1/16 1/8])))
      (cutoff-control :cutoff2 grp2 (/- 1/4 (rnd 0.5 4.0))  (rnd 0.1 0.8) 0)

      (to: :root -5)
      (to: :scale minor))))
