
(start)
(stop)

(pattern metro
  (in! (sbdv 4)
       (to: :scd (sbdv [V I I I])
            :octave 1
            :amp 0.1)))

(pattern tps
  (in! (sbdv [1 ~ ~ ~])
       (taps 1/16 -8)
       (to: :scd III
            :sustain 1/16)))

(context-events-next (testc tps))