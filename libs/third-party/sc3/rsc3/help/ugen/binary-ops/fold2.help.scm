;; (fold2 a b)

;; Bilateral folding.  folds a to +/- b.

(audition
   (out 0 (fold2 (f-sin-osc ar 1000 0)
		 (line kr 0 1 8 do-nothing))))
