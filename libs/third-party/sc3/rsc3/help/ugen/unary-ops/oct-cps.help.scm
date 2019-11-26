;; (oct-cps a)

;; Convert decimal octaves to cycles per second.

(audition
 (let ((f (oct-cps (line kr 2 9 6 remove-synth))))
   (out 0 (mul (saw ar f) 0.2))))

(audition 
 (let ((f (oct-cps (u:round (line kr 2 9 6 remove-synth) (/ 1 12)))))
   (out 0 (mul (saw ar f) 0.2))))
