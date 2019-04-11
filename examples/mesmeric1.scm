
(define arp-pattern
  (o->
    (in: :scd (/- [I V VII])
	 (to: :octave -1))
    
    (in: :scd (/- 3/2 [I V VII])
	 (to: :octave (/- 4 [1 0 2 0])))
    
    (in: :scd (/- 1/4 [I V VII])
	 (to: :amp 0.1
	      :pan (/- 1/8 [0.3 0.7])))

    (to: :root (/- 16 [I III])
	 :inst "swirly-keys"
	 :sustain 0
	 :attack 0.0025
	 :release (sine 12 0.25 1.75)
	 :bus1-amt 0.3
	 :cutoff (/- 4/5 [0.9 2.9 1.3]))

    (to+ :cutoff (sine 7 -0.4 5.0))))

(define pad-pattern
  (o->
    (in: :scd (/- 8 [I VII])
	 (to: :root (/- 16 [I III])
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

(define drum-pattern
  (o->
    (in* (/- [1 [~ 1] ~ (pick [1 1 2 [~ 1]]) (pick [~ 1 [~ 1]]) (pick [~ 1])])
	 (to: :sample bd
	      :amp 0.2)
	 (to* :sustain 0.6))

    (in* (/- 1/2 [~ (each 2/3 [~ 1]) ~ ~ (each 3/4 [1 ~ [~ 1] ~ 1]) ~])
	 (to: :sample xt)
	 (to: :amp 0.5))
    
    (in* (/- 1/2 [1 1 1 1 1 1])
	 (to: :sample hh)
	 (to: :amp (/- 1/4 [0.05 (rnd 0.2 0.3)]))
	 (to: :pan (rnd 0.4 0.6)))))

(define p1
  (o->
    arp-pattern
    drum-pattern
    ;; pad-pattern
    ))
