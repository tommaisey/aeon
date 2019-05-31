; default values
(hear (mul (logistic ar 3 1000 0.5) 0.1))

; onset of chaos
(hear (mul (logistic ar (line kr 3.55 3.6 5 do-nothing) 1000 0.5) 0.1))

; explore via mouse
(hear
 (let ((x (mouse-x kr 3 3.99 0 0.1))
       (y (mouse-y kr 10 10000 exponential 0.1)))
   (mul (logistic ar x y 0.25) 0.1)))