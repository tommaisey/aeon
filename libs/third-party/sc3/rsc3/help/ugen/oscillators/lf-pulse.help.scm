(let ((f (mul-add (lf-pulse kr 3 0 0.3) 200 200)))
  (audition (out 0 (mul (lf-pulse ar f 0 0.2) 0.1))))
