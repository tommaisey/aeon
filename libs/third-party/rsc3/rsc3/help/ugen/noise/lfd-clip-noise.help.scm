;; (lfdclip-noise rate freq)

;; Like lfclip-noise, it generates the values -1 or +1 at a rate given
;; by the freq argument, with two differences: no time quantization,
;; and fast recovery from low freq values.

;; (lfclip-noise, as well as lf-noise0,1,2 quantize to the nearest
;; integer division of the samplerate, and they poll the freq argument
;; only when scheduled, and thus seem to hang when freqs get very
;; low).

;; If you don't need very high or very low freqs, or use fixed freqs,
;; lfclip-noise is more efficient.

;; Try wiggling mouse quickly; LFNoise frequently seems stuck,
;; LFDNoise changes smoothly.

(let ((f (mul-add (lfclip-noise ar (mouse-x kr 0.1 1000 1 0.1)) 200 500)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))

(let ((f (mul-add (lfdclip-noise ar (mouse-x kr 0.1 1000 1 0.1)) 200 500)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))

;; LFNoise quantizes time steps at high freqs, LFDNoise does not:

(let ((f (x-line kr 1000 20000 10 remove-synth)))
  (audition (out 0 (mul (lfclip-noise ar f) 0.1))))

(let ((f (x-line kr 1000 20000 10 remove-synth)))
  (audition (out 0 (mul (lfdclip-noise ar f) 0.1))))
