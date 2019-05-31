;; (cps-oct a)

;; Convert cycles per second to decimal octaves.

(audition 
 (let ((f (oct-cps (cps-oct (x-line kr 600 900 6 remove-synth)))))
   (out 0 (mul (saw ar f) 0.2))))
