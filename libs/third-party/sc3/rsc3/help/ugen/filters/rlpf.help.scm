;; (Rlpf in freq rq)

;; A resonant low pass filter.

(let* ((f1 (x-line kr 0.7 300 20 remove-synth))
       (f2 (mul-add (f-sin-osc kr f1 0) 3600 4000)))
  (audition 
   (out 0 (rlpf (mul (saw ar 200) 0.1) f2 0.2))))
