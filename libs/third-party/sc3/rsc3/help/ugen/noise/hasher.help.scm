;; (hasher in)

;; Returns a unique output value from zero to one for each input value
;; according to a hash function. The same input value will always
;; produce the same output value. The input need not be from zero to
;; one.

(audition (out 0 (mul (hasher (line ar 0 1 1 2)) 0.2)))
