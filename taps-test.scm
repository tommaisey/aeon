
(start)
(stop)

(pattern metro
  (in! (sbdv 4)
       (to: :scd (sbdv [V I I I])
            :octave 1
            :amp 0.1)))

(pattern tps
  (in! (sbdv [1 ~ ~ ~])
       (to: :amp 1
            :scd III)
       (taps 1/16 -8 (to* :amp 0.7) (to: :scd IV))
       (to: :sustain 1/16)))

(context-events-next (testc tps))