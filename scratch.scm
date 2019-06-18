(start)
(pause)
(clear-all)
(set-bpm! 100)

(pattern bass
  (in! (sbdv [[~ 1] [~ 1 » »] 1 [~ ~ 1 »]])
       (to: :scd (sbdv 2 [II I II I])
            :octave -1
            :root Bb
            :scale pent-major
            :attack (every 24 1/8 [0.005 1/6])
            :amp 0.4)
       main-swing
       (mv+ 1/110)))