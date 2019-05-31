;; (sos in a0 a1 a2 b1 b2)

;; Second order filter section (biquad).  A standard second order
;; filter section. Filter coefficients are given directly rather than
;; calculated for you.

;; Same as two-pole

(let* ((theta (line kr (* 0.2 pi) pi 5 remove-synth))
       (rho (line kr 0.6 0.99 5 remove-synth))
       (b1 (mul 2 (mul rho (u:cos theta))))
       (b2 (neg (squared rho))))
  (audition (out 0 (sos (lf-saw ar 200 0.1) 1 0 0 b1 b2))))
