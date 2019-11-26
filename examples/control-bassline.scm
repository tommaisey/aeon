

(define grp1 (make-unused-group-id))

(pattern p1
  (o->
    (in! (over 2 1)
         (to: :group grp1
              :attack 0.01
              :release 0.1
              :amp 0.3
              :inst "dual-lopass"))

    (in: :chd (over 1/2 [I I (? [I III]) I])
         (to+ :chd (over 4 [I I I V]))
         (to: :group grp1 :control "pitchbend"
              :scd (over 2 [I (each 2 [VI III])])
              :octave (over 2 [-1 (? [0 1]) (? [0 1 -1])])
              :bus1-amt (rnd 0.0 0.125)
              :pan (over 1/2 [0.4 0.6]))
         (mv* (? [1 3/2 2])))

    (in: :cutoff1 (over 1/4 (? 4.0 12.0))
         (to: :group grp1 :control "cutoff"
              :resonance (? 0.3 1.8)))

    (in: :cutoff2 (over 1/8 (? 1.0 3.0))
         (to: :group grp1 :control "cutoff"
              :resonance (? 0.1 0.8))
         (mv+ (over 1/4 (? [0 0 1/16 1/8]))))

    (to: :root 55
         :scale minor)))
