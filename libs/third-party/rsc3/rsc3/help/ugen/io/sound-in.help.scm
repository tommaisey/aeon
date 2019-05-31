;; (sound-in channel)

;; Read audio from the sound input hardware.

;; channel - input channel number to read, 
;;           indexed from zero, can be mce.

(audition (out 0 (sound-in 0)))

(audition (out 0 (sound-in (mce2 0 1))))

(audition (out 0 (sound-in (mce4 0 2 1 3))))
