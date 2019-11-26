(hear (mul (white-noise ar) 0.15))

;; Noise generators constructors are unique, to share noise UGens
;; values must be explictly stored and reused.

(hear (mul (sub (white-noise ar) (white-noise ar)) 0.15))

(hear (let ((n (white-noise ar))) (sub n n)))
