;; (rand-seed rate trig seed)
 
;; When the trigger signal changes from nonpositive to positve, the
;; synth's random generator seed is reset to the given value. All
;; other synths that use the same random number generator reproduce
;; the same sequence of numbers again.  

;; See also: randID.
 
;; Start a noise patch 

(let ((n (add (mul (white-noise ar) (mce2 0.05 0.05)) (dust2 ar (mce2 70 70))))
      (f (mul-add (lf-noise1 kr 3) 5500 6000)))
  (audition (out 0 (add (resonz (mul n 5) f 0.5) (mul n 0.5)))))

;; Reset the seed at a variable rate.

(audition (mrg2 (rand-seed kr (impulse kr (mouse-x kr 0.1 100 0 0.1) 0) 1956)
		0))
