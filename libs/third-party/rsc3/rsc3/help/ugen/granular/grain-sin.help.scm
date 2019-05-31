(hear
 (let* ((x (mouse-x kr -0.5 0.5 0 0.1))
        (y (mouse-y kr 0 400 0 0.1))
        (n (white-noise kr))
        (f (add 440 (mul n y)))
        (t (impulse kr 10 0)))
   (mul (grain-sin 2 t 0.1 f x -1 512) 0.1)))
