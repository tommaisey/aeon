(hear
 (let ((a (mce3 (sin-osc ar 220 0)
                (saw ar 440)
                (pulse ar 110 0.1)))
       (t (dust ar (mouse-x kr 1 1000 1 0.1)))
       (w (mce3 0.6 0.15 0.05)))
   (mul (tw-choose t a w 1) 0.1)))
