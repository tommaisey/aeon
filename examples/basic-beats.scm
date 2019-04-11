

(define kik1
  (in: :sample (/- [bd [~ bd bd ~] [~ bd ~ bd] [~ ~ ~ bd]])
       (to: :amp (/- [0.2 [0.05 0.2] [0.2 0.1] » » »]))))

(define kik2
  (in: :sample (/- [bd [~ ~ bd bd] [~ ~ bd bd] [~ bd ~ bd]])
       (to: :amp (/- [0.2 [0.05 0.2] [0.2 0.1] » »]))))

(define p1
  (o->
    (rp: (/- 2 [! kik1 kik2]))
    
    (in: :sample (/- [~ sn ~ sn])
	 (to: :amp 0.15
	      :pan 0.4
	      :speed 0.85
	      :sustain 0.01))

    (in: :amp (/- 1/8 [0.015 (rnd 0.03 0.05)])
	 (to: :sample hh)
	 (to: :pan (rnd 0.6 0.7)))

    (mute
     (in: :amp (/- 1/4 [~ 0.1])
	  (to: :sample oh
	       :pan 0.6)))

    (to* :amp (rnd 0.7 1.02))))
