;; (local-in num-channels rate)

;; Define and read from buses local to a SynthDef

;; num-channels - the number of channels of local buses.

;; Localin defines buses that are local to the SynthDef. These are
;; like the global buses, but are more convenient if you want to
;; implement a self contained effect that uses a feedback processing
;; loop.  There can only be one audio rate and one control rate
;; Localin per SynthDef.  The audio can be written to the bus using
;; local-out.

(let* ((a0 (mul (decay (impulse ar 0.3 0) 0.1) (mul (white-noise ar) 0.2)))
       (a1 (add (local-in 2 ar) (mce2 a0 0)))
       (a2 (delay-n a1 0.2 0.2)))
  (audition (mrg2 (local-out (mul (mce-reverse a2) 0.8)) 
		  (out 0 a2))))
