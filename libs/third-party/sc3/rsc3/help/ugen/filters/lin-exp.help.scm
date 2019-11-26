;; (lin-exp in srclo srchi dstlo dsthi)

;; Map a linear range to an exponential range.

;; in    - input to convert            - kr, ar
;; srclo - lower limit of input range  - ir
;; srchi - upper limit of input range  - ir
;; dstlo - lower limit of output range - ir
;; dsthi - upper limit of output range - ir

(audition
 (out 0 (mul (sin-osc ar (lin-exp (mouse-x kr 0 1 0 0.1) 0 1 440 660) 0)
	     (lin-exp (mouse-y kr 0 1 0 0.1) 0 1 0.01 0.25))))
