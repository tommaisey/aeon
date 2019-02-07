;; After loading init.scm, play around in this file.
;; Ctrl-C-C re-evaluates the definition you're in.

(start)
(pause)

(define *1
  (subdivide
   [event 2 [[2 (rnd 1 5) 2 4] [(rnd 0 6) 3 1 ~]]]
   ['freq 1 [220 330 110]]
   ['length 3 [0.8 0.4 1.2 0.3]]
   
   [event [~ (pick [3 6 8]) 2 1]]
   ['freq 3 [(* 110 3) 880 990 (* 110 6)]]
   ['freq 3/2 [~ [~ 440]]]))


(define *1
  (subdivide
   (event 1 [1 (rnd 3 16)])
   ('freq 3/2 [660 [330 440] 220])))
