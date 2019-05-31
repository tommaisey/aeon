;; (hpf in freq)

;; Second order Butterworth highpass filter.

(let* ((i (mul (saw ar 200) 0.1))
       (f1 (x-line kr 0.7 300 20 do-nothing))
       (f2 (mul-add (f-sin-osc kr f1 0) 3600 4000)))
  (audition (out 0 (mul (hpf i f2) 5))))
