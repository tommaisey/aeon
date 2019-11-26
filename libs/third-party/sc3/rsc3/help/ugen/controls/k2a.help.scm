(hear (k2a (mul (white-noise kr) 0.3)))

(hear (mce2 (k2a (mul (white-noise kr) 0.3))
            (mul (white-noise ar) 0.3)))

(hear
 (let* ((block-size (fdiv sample-rate control-rate))
        (freq (mul (fdiv (mouse-x kr 0.1 40 1 0.1) block-size) sample-rate)))
   (mul (mce2 (k2a (lf-noise0 kr freq))
              (lf-noise0 ar freq))
        0.3)))
