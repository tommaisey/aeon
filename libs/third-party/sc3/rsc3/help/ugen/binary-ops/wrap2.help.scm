;; (wrap2 a b)

;; Bilateral wrapping.  wraps input wave to +/- b.

(audition
 (out 0 (wrap2 (f-sin-osc ar 1000 0) 
	       (line kr 0 1.01 8 do-nothing))))
