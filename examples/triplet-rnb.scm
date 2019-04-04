

;; A groovy little number...

(define p1
  (o->
    (o-> ;; Keys
      (in: :scd (/- 2 [[I III] I VI •])
	   (to: :octave (/- [0 1 (pick [1 0]) 0]))
	   (rp: (/- 4 [! triad 7th 9th])))

      (cp: (to: :octave -1)
	   (to+ :beat 1/12))

      (in: :scd (/- [I III IV])
	   (to: :octave 1))

      (in: :scd (/- 2 [I [I III] IV II])
	   (to: :octave -1))

      (to: :scale dorian
	   :inst "swirly-keys"
	   :attack 0.005
	   :sustain 0
	   :release (/- [2 1 1 4]))

      (to* :release 0.3)
      
      (o-> ;; Drums
	(in: :sample (/- 1/3 [• hh × ×])
	     (to: :amp 0.1
		  :pan (rnd 0.1 0.7)))
	
	(in* (/- 2 [(rnd 1 3) 1 [1 1] 1])
	     (to: :sample bd))
	
	(in: :sample (/- 1 [• sn • sn])
	     (to: :amp 0.2))))))
