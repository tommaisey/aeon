;; (cps-midi a)

;; Convert cycles per second to MIDI note.

(let ((f (line kr 600 900 5 remove-synth)))
  (audition 
   (out 0 (mul (saw ar (midi-cps (cps-midi f))) 0.1))))
