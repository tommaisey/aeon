

(define kik1
  (in: :sample (sbdv [bd [~ bd bd ~] [~ bd ~ bd] [~ ~ ~ bd]])
       (to: :amp (sbdv [0.2 [0.05 0.2] [0.2 0.1] » » »]))))

(define kik2
  (in: :sample (sbdv [bd [~ ~ bd bd] [~ ~ bd bd] [~ bd ~ bd]])
       (to: :amp (sbdv [0.2 [0.05 0.2] [0.2 0.1] » »]))))

(defpattern p1
  (o->
    (rp: (sbdv 2 [! kik1 kik2]))

    (in: :sample (sbdv [~ sn ~ sn])
         (to: :amp 0.15
              :pan 0.4
              :speed 0.85
              :sustain 0.01))

    (in: :amp (sbdv 1/8 [0.015 (rnd 0.03 0.05)])
         (to: :sample hh
              :pan (rnd 0.6 0.7)))

    (mute
     (in: :amp (sbdv 1/4 [~ 0.1])
          (to: :sample oh
               :pan 0.6)))

    (to* :amp (rnd 0.7 1.02))))
