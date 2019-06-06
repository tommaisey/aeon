
(pattern arp-pattern
  (o->
    (in: :scd (sbdv [I V VII])
         (to: :octave -1))

    (in: :scd (sbdv 3/2 [I V VII])
         (to: :octave (sbdv 4 [1 0 2 0])))

    (in: :scd (sbdv 1/4 [I V VII])
         (to: :amp 0.1
              :pan (sbdv 1/8 [0.3 0.7])))

    (to: :root (sbdv 16 [I III])
         :inst "swirly-keys"
         :sustain 0
         :attack 0.0025
         :release (sine 12 0.25 1.75)
         :bus1-amt 0.3
         :cutoff (sbdv 4/5 [0.9 2.9 1.3]))

    (to+ :cutoff (sine 7 -0.4 5.0))))

(pattern pad-pattern
  (o->
    (in: :scd (sbdv 8 [I VII])
         (to: :root (sbdv 16 [I III])
              :inst "dual-lopass"
              :attack 4
              :release 12
              :amp 0.05
              :tremolo-speed (pick [0.75 1 1.5 2 4 6 8]))
         (mv- 1/8)
         (rp: triad)
         (to: :resonance (rnd 0.1 1.2)
              :pan (rnd 0.1 0.9))
         (cp: (to: :octave 1)))))

(pattern drum-pattern
  (o->
    (in* (sbdv [1 [~ 1] ~ (pick [1 1 2 [~ 1]]) (pick [~ 1 [~ 1]]) (pick [~ 1])])
         (to: :sample bd
              :amp 0.2)
         (to* :sustain 0.6))

    (in* (sbdv 1/2 [~ (each 2/3 [~ 1]) ~ ~ (each 3/4 [1 ~ [~ 1] ~ 1]) ~])
         (to: :sample xt)
         (to: :amp 0.5))

    (in* (sbdv 1/2 [1 1 1 1 1 1])
         (to: :sample hh)
         (to: :amp (sbdv 1/4 [0.05 (rnd 0.2 0.3)]))
         (to: :pan (rnd 0.4 0.6)))))
