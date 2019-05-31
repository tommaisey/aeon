;; (last-value in diff) 			 
 
;; output the last value before the input changed more than a
;; threshhold.
 
(let ((f (last-value (mouse-x kr 100 400 0 0.1) 40)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))

(let* ((x (mouse-x kr 0.1 4 0 0.1))
       (f (mul-add (u:abs (sub x (last-value x 0.5))) 400 200)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.2))))
