;; Since the allpass delay has no audible effect as a resonator on
;; steady state sound ...

(define z (mul (white-noise ar) 0.1))

(audition (out 0 (allpass-c z 0.01 (x-line kr 0.0001 0.01 20 do-nothing) 0.2)))

;; ...these examples add the input to the effected sound so that you
;; can hear the effect of the phase comb.

(hear (add z (allpass-n z 0.01 (x-line kr 0.0001 0.01 20 do-nothing) 0.2)))

(hear (add z (allpass-l z 0.01 (x-line kr 0.0001 0.01 20 do-nothing) 0.2)))

(hear (add z (allpass-c z 0.01 (x-line kr 0.0001 0.01 20 do-nothing) 0.2)))

;; Used as an echo - doesn't really sound different than Comb, but it
;; outputs the input signal immediately (inverted) and the echoes are
;; lower in amplitude.

(hear (allpass-n (mul (decay (dust ar 1) 0.2) z) 0.2 0.2 3))
