;; (out bufferindex inputs)				

;; Send signal to an audio or control buss, mix with existing signal.
;; The user is responsible for making sure that the number of channels
;; match and that there are no conflicts.

(audition (out 0 (mul (sin-osc ar (mce2 330 331) 0) 0.1)))
