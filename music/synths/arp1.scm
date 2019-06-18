(pattern beep-arp
  (let ([v 24])
    (in! (sbdv v)
         (to: :scd (step (/ 1 v) [0 5 2 3 4 2 6 9 8 1])
              :octave (sbdv 4 [-1 0 1])
              :scale pent-major
              :root Bb
              :amp 0.11
              :inst "swirly-keys"
              :bus1-amt (sine 8 0 0.5)
              :bus2-amt (every 24 (/ 1 v) [0 (rnd 0.2 0.7)])
              :attack 0.005
              :release (/ 1 v)
              :sustain 0)
         main-swing)))