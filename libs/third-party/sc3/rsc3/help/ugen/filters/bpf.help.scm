;; (bpf in freq rq) 

;; Second order Butterworth bandpass filter

;; in    - input signal to be processed
;; freq  - cutoff frequency in Hertz.
;; rq    - the reciprocal of Q.  bandwidth / cutoffFreq. 

(let* ((f1 (x-line kr 0.7 300 20 remove-synth))
       (f2 (mul-add (f-sin-osc kr f1 0) 3600 4000)))
  (audition (out 0 (bpf (mul (saw ar 200) 0.25) f2 0.3))))

(let* ((f1 (mouse-x kr 220 440 0 0.1))
       (f2 (mce2 f1 (sub 550 f1)))
       (rq (mouse-y kr 0 0.01 0 0.1)))
  (audition (out 0 (bpf (white-noise ar) f2 rq))))
