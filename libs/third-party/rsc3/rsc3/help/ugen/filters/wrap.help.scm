;; (wrap in lo hi)

;; wrap a signal outside given thresholds.

;; This differs from the BinaryOpUGen wrap2 in that it allows one to
;; set both low and high thresholds.

;; in - signal to be wrapped
;; lo - low threshold of wrapping
;; hi - high threshold of wrapping

(let ((o (mul (sin-osc ar 440 0) 0.2))
      (l (rand -0.175 -0.025))
      (r (rand 0.025 0.175)))
  (audition (out 0 (wrap o l r))))

;; lo and hi are i-rate only.

(let ((o (mul (sin-osc ar 440 0) 0.2))
      (x (mouse-x kr -0.175 -0.025 1 0.1))
      (y (mouse-y kr 0.025 0.175 1 0.1)))
  (audition (out 0 (wrap o x y))))
