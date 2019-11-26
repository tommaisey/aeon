;; (ti-rand lo hi trig)

;; Generates a random integer value in uniform distribution from lo to
;; hi each time the trig signal changes from nonpositive to positive
;; values

(let ((p (ti-rand -1 1 (dust kr 10))))
  (audition (out 0 (pan2 (pink-noise ar) p 0.2))))

(let ((f (mul-add (ti-rand 4 12 (dust kr 10)) 150 (mce2 0 1))))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
