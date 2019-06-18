

(define kik1
  (in! (s1v [1 [~ 1 1 ~] [~ 1 ~ 1] [~ ~ ~ 1]])
       (to: :amp (sbdv [0.2 [0.05 0.2] [0.2 0.1] » » »]))))

(define kik2
  (in: :sample (sbdv [1 [~ ~ 1 1] [~ ~ 1 1] [~ 1 ~ 1]])
       (to: :amp (sbdv [0.2 [0.05 0.2] [0.2 0.1] » »]))))

(pattern p1
  (o->
    (rp: (sbdv 2 [! kik1 kik2]))
    (to: :sample bd
         :sample-idx 81)

    (in: :sample (sbdv [~ sn ~ sn])
         (to: :sample-idx 18
              :amp 0.15
              :pan 0.4
              :speed 0.85
              :sustain 0.01))

    (in: :amp (sbdv 1/8 [0.015 (rnd 0.03 0.05)])
         (to: :sample hh
              :sample-idx 12
              :pan (rnd 0.6 0.7)))

     (off in: :amp (sbdv 1/4 [~ 0.1])
          (to: :sample oh
               :pan 0.6))

    (to* :amp (rnd 0.7 1.02))))