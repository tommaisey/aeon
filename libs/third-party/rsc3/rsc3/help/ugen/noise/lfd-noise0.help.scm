;; (lfd-noise0 rate freq)
;; (lfd-noise1 rate freq)
;; (lfd-noise3 rate freq)

;; lfd-noise0: Dynamic step noise. Like lf-noise0, it generates random
;; values at a rate given by the freq argument, with two differences:
;; no time quantization, and fast recovery from low freq values.

;; lfd-noise1: Dynamic ramp noise. Like lf-noise1, it generates linearly
;; interpolated random values at a rate given by the freq argument,
;; with two differences: no time quantization, and fast recovery from
;; low freq values.

;; lfd-noise3: Dynamic cubic noise. Like Lf-Noise3, it generates
;; linearly interpolated random values at a rate given by the freq
;; argument, with two differences: no time quantization, and fast
;; recovery from low freq values.

;; (lf-noise0,1,3 quantize to the nearest integer division of the
;; samplerate, and they poll the freq argument only when scheduled,
;; and thus seem to hang when freqs get very low).

;; If you don't need very high or very low freqs, or use fixed freqs,
;; lf-noise0,1,3 is more efficient.

;; Try wiggling mouse quickly; Lf-Noise frequently seems stuck,
;; LFDNoise changes smoothly.

(audition
 (out 0 (mul (lf-noise0 ar (mouse-x kr 0.1 1000 1 0.1)) 0.1)))

(audition
 (out 0 (mul (lfd-noise0 ar (mouse-x kr 0.1 1000 1 0.1)) 0.1)))

;; silent for 2 secs before going up in freq

(audition
 (out 0 (mul (lf-noise0 ar (x-line kr 0.5 10000 3 remove-synth)) 0.1)))

(audition
 (out 0 (mul (lfd-noise0 ar (x-line kr 0.5 10000 3 remove-synth)) 0.1)))

;; lf-noise quantizes time steps at high freqs, lfd-noise does not:

(audition
 (out 0 (mul (lf-noise0 ar (x-line kr 1000 20000 10 remove-synth)) 0.1)))

(audition
 (out 0 (mul (lfd-noise0 ar (x-line kr 1000 20000 10 remove-synth)) 0.1)))
