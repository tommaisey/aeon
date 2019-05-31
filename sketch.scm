(current-directory "~/Code/convex")
(load "init/init.scm")

(start)
(pause)

(defpattern tune1
  (in! 16
       (to: :scd (step 1/4 [I V III (pick [IV VI VII VIII])])
            :inst "sine-grain"
            :bus1-amt (sine 3 0.05 0.6)
            :pan (rnd 0.24 0.75)
            :attack 0.005)
       ;; (cp: (to: :octave (pick [-1 1 2])))
       ))


(defpattern p1
  (o->
    (in: :scd (step 1/8 [I [VI II] [III ~ V ~] III VII])
         (to: :inst "sine-grain"
              :amp (sbdv 1/3 [0.2 0.4 (rnd 0.1 0.5)])
              :attack 0.005
              :bus1-amt 0.1
              :octave (pick [0 -1 1])
              :pan (rnd 0.25 0.75))
         (to* :sustain (sbdv [0.5 2 1 0.5])))
    (o->
      (in: :sample (sbdv 1 [bd [sn bd] [~ bd ~ bd] sn])
           (to: :sustain (pick [1/8 1/32 1/4 1/16])))
      (in: :sample (sbdv 1/2 [~ hh ~ [~ hh]])
           (to: :sustain 0.01))
      (to: :inst "sampler"
           :bus1-amt 0.02
           :amp (sbdv 1/4 [0.3 (rnd 0.1 0.2)])))))

(defpattern p1
  (define mid-bd
    (pick [[bd ~ bd bd] [~ bd bd ~]]))
  (o->
    (in: :scd (step 1/16 [I II III V » V VIII VII])
         (to: :chd (step 2 [I IV I II])
              :pan (sine 3/2 0.3 0.7)))
    (in: :scd (sbdv [I I I I])
         (to: :octave -1
              :chd (sbdv 4 [I IV VI III])))
    (to: :inst "swirly-keys"
         :sustain 1/24
         :attack 0.001
         :release (sine 4 0.05 0.5)
         :bus2-amt (sine 3 0.05 0.15))

    (o->
      (in: :sample (sbdv [bd [~ bd ~ ~] mid-bd bd])
           (to: :sustain 1/16))
      (in: :sample (sbdv [~ (pick [sn [sn ~ ~ sn]]) ~ sn])
           (to* :sustain 1/16)
           (to* :release (rnd 0.8 2.0)))
      (in: :sample (step 1/16 [hh hh ~ hh ~])
           (to: :amp 0.075))
      (in: :sample (step 1/16 [~ ~ ~ ~ oh ~ ~ ~ ~])
           (to: :amp 0.1))
      (to: :inst "sampler"))))

(defpattern p1
  (o->
    (in: :sample (sbdv [bd [~ bd bd ~] [~ bd ~ bd] [~ ~ ~ bd]]))
    (in: :sample (sbdv [~ sn ~ sn])
         (to: :amp 0.15))

    (in: :amp (sbdv 1/8 [0.05 0.1])
         (to: :sample hh))
    (in: :amp (sbdv 1/4 [~ 0.24])
         (to: :sample oh
              :pan 0.7
              :amp 0.175))
    (to: :inst "sampler")
    (to* :sustain (sbdv 4 [0.5 0.05 1 0.01]))))

;; A little beat
(defpattern p1
  (let ([bd1 (in! (sbdv [4 2 [1 ~] 1]))]
        [bd2 (in! (sbdv [1 2 [2 ~] 1]))])
    (o->
      (in! (sbdv 2 [! bd2 bd1])
           (to: :sample bd
                :amp (sbdv 1/2 [0.25 (rnd 0.01 0.1) » »])
                :speed 1))

      (in! (sbdv [1 4 (pick [~ 2 4 3]) 1])
           (to: :sample hh
                :amp (sbdv 1/2 [0.1 0.1 0.25 0.1])
                :speed (rnd 0.95 1.05)
                :pan (rnd 0.6 0.8)
                :bus1-amt (rnd 0.001 0.015)))

      (in! (sbdv [~ 1 ~ 1])
           (to: :sample sn
                :pan 0.45
                :speed 1
                :bus1-amt 0.02))

      (in! (sbdv [~
                  [(pick [3 2 4 ~]) (pick [2 4 ~])]
                  (pick [~ 1 3])
                  [~ (pick [~ 1 2])]])
           (to: :sample xt
                :amp (sbdv 1/2 [0.15 (rnd 0.1 0.4) (rnd 0.15 0.4) 0.15])
                :speed (rnd 0.8 0.85)
                :pan (rnd 0.2 0.8)
                :bus1-amt (rnd 0.0 0.05)))

      (to: :speed (sbdv [~ 2]))

      (to: :inst "sampler"))))
