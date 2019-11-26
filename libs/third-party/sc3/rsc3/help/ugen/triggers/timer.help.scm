;; (timer trig)

;; Returns time since last triggered
	
;; Using timer to modulate sine frequency: the slower the trigger is
;; the higher the frequency

(let* ((t (impulse kr (mouse-x kr 0.5 20 1 0.1) 0))
       (s (sin-osc ar (mul-add (timer t) 500 500) 0)))
  (audition (out 0 (mul s 0.2))))
