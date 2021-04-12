;; A bit of sawtooth inspiration
(let ([prog (over 8 [I III VI V])])
  (pattern fake-inspiration
    ;; bass
    (in! (euc 16 13)
         (to: :inst "saw-grain"
              :octave -2
              :amp 0.15
              :cutoff (sine 8 0.3 0.55)
              :pan (over 1/4 [0.45 0.55])
              :scd prog)
         (legato)
         (to* :sustain 3))
    
    ;; melody
    (in! (over 2 [(euc 8 5 1) ~])
          (to: :inst "saw-grain"
               :cutoff (sine 15 0.3 0.4)
               :octave -1
               :scd prog
               :chd (over 1/2 [5 4 2 0])))))

