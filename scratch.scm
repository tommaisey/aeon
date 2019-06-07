(start)
(pause)
(clear-all)
(set-bpm! 110)

(samples-dir cy
  (string-append 
   "/Users/tommaisey/Dropbox/Music Production/"
   "Samples & Patches/CC Samples/Real Instruments/Ghana Bells"))

(samples-dir wind
  (string-append
   "/Users/tommaisey/Dropbox/Music Production/Samples & Patches/"
   "CC Samples/Real Instruments/Woodwind"))

(define main-swing (swing 1/16 0.2))

(define rev-sample
  (x-> (to* :speed -1
            :sustain 2)
       (to: :sample-pos (rnd 0.1 0.2))))

(pattern bells
  (o->
    (in! (sbdv [1 [~ ~ 1 ~] [~ 1 ~ 1] 1])
         (to: :sample cy
              :sample-idx (sbdv [2 15 (rnd 10 14) (rnd 28 30)])
              :amp 1.5
              :bus1-amt (rnd 0.1 0.9)
              :bus2-amt (sbdv 1/4 [(rnd 0.0 0.5) 0])
              :pan (rnd 0.25 0.75)
              :speed (pick [0.25 0.5 1 2])
              :attack 0.005
              :release 1/2)
         (to* :speed (rnd 0.98 1.01))
         (off rev-sample))

    (let ([tt 8])
      (in! tt
           (to: :sample cy
                :sample-idx (step (/ 1 tt) [23 (every 4 1/2 [29 12 15 18]) 14])
                :amp (step (/ 1 tt) [0.1 0.5 0.1 0.1 0.3])
                :pan (rnd 0.4 0.8)
                :bus1-amt 0.2
                :speed 1)
           (to* :sustain 0.75)))
    main-swing))

(pattern wind1
  (in! 16
       (to: :sample wind
            :sample-idx (step 1/16 [3 10 8 1 5 9 6])
            :sample-pos (step 1/16 [0.13 0.43 0.23 0.75 0.83])
            :attack 0.002
            :sustain 1/16
            :release 1/8
            :bus2-amt (every 48 1/16 [0 0.3])
            :amp (sbdv 1/4 [0.2 0.5])
            :speed (pick [0.5 1 2]))
       (to+ :sample-pos (rnd 0.0 0.15))
       main-swing))

(pattern beep-arp
  (let ([v 24])
    (in! (sbdv v)
         (to: :scd (step (/ 1 v) [0 5 2 3 4 2 6 9 8 1])
              :octave (sbdv 4 [-1 0 1])
              :scale pentMajor
              :root Bb
              :amp 0.11
              :inst "swirly-keys"
              :bus1-amt (sine 8 0 0.5)
              :bus2-amt (every 24 (/ 1 v) [0 (rnd 0.2 0.7)])
              :attack 0.005
              :release (/ 1 v)
              :sustain 0))))

(pattern hh1
  (in! 16
       (to: :sample hh
            :sample-idx (sbdv 1/8 [30 12])
            :amp (sbdv 1/4 [0.01 0.03])
            :pan (sbdv 1/4 [0.3 0.7]))
       main-swing))

(pattern bd1
  (in! (sbdv [1 1 1 1])
       (to: :sample (bd/ 80)
            :amp 0.2
            :speed 0.84
            :sustain 1/4)
       main-swing))

(pattern bass
  (in! (sbdv [[~ 1] [~ 1 » »] 1 [~ ~ 1 »]])
       (to: :scd (sbdv 2 [II I II I])
            :octave -1
            :root Bb
            :scale pentMajor
            :attack (every 24 1/8 [0.005 1/6])
            :amp 0.4)
       main-swing
       (mv+ 1/110)))

(pattern sn1
  (o->
    (in! (sbdv [~ 1 ~ 1])
         (to: :sample (sn/ 78)
              :amp 0.4
              :bus1-amt 0.03))

    (in! (every 6 1 [~
                     [~ ~ [~ ~ 1 ~] [~ ~ 1 ~]]
                     [~ ~ [~ ~ 1 1] [1 ~ 1 1]]])
         (to: :sample (sn/ 28)
              :amp 0.3
              :bus1-amt 0.08))
    main-swing))