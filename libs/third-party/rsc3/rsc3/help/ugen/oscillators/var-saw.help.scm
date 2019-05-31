(hear
 (let ((f (mul-add (lf-pulse kr (mce2 3 3.03) 0 0.3) 200 200))
       (w (lin-lin (lf-tri kr 1 0) -1 1 0 1)))
   (mul (var-saw ar f 0 w) 0.1)))
