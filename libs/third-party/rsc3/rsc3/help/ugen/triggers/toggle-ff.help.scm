;; (toggle-ff trig)

;; Toggle flip flop. Toggles between zero and one upon receiving a trigger.

;; trig - trigger input

(let* ((t (dust ar (x-line kr 1 1000 60 do-nothing)))
       (s (sin-osc ar (mul-add (toggle-ff t) 400 800) 0)))
  (audition (out 0 (mul s 0.1))))
