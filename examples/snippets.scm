;; A bit of sawtooth inspiration
(let ([prog (over 8 [I III VI V])])
  (pattern fake-inspiration
    ;; bass
    (syn "saw-grain" (euc 16 13)
         (to: :octave -2
              :amp 0.15
              :cutoff (sine 8 0.3 0.55)
              :pan (over 1/4 [0.45 0.55])
              :scd prog)
         (legato)
         (to* :sustain 3))

    ;; melody
    (syn "saw-grain" (over 2 [(euc 8 5 1) ~])
          (to: :cutoff (sine 15 0.3 0.4)
               :octave -1
               :scd prog
               :chd (over 1/2 [5 4 2 0])))))

;; A little drum groove
(pattern drums
  (syn "bd" (euc 8 5)
    (to: :scd V :amp 0.6)
    (legato 1/2))

  (syn "hh" (euc 16 13 2)
    (to: :amp (over 1/8 [0.1 0.3])
         :sustain (over 1/8 [~ (? [1/6 1/8 1/4])])))

  (syn "cp" (euc 8 2 3)
    (to: :freq 60
         :sustain (over 2 [(? [1/8 1/4 1/2]) 1/16 !4]))
    (sq: (over 1/2 (? [~ ~ (taps 1/16 2) (taps 1/16 -3)])))
    (to: :amp (over 1/2 [(? 0.1 0.25) 0.4])))

  (to* :amp 0.4)
  (tt+ 1/4)
  (swing 1/16 0.08))

