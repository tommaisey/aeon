(audition (out 0 (mul (lf-saw ar 500 1) 0.1)))

;; Used as both oscillator and LFO.

(let ((f (mul-add (lf-saw kr 4 0) 400 400)))
  (audition (out 0 (mul (lf-saw ar f 0) 0.1))))
