;; (replace-out bufferindex inputs)

;; Send signal to a bus, overwrite existing signal.

(audition
 (mrg3 (out 0 (mul (sin-osc ar (mce2 330 331) 0) 0.1))
       (replace-out 0 (mul (sin-osc ar (mce2 880 881) 0) 0.1))
       (out 0 (mul (sin-osc ar (mce2 120 121) 0) 0.1))))

;; Compare to:

(audition
 (mrg3 (out 0 (mul (sin-osc ar (mce2 330 331) 0) 0.1))
       (out 0 (mul (sin-osc ar (mce2 880 881) 0) 0.1))
       (out 0 (mul (sin-osc ar (mce2 120 121) 0) 0.1))))
