(import (rsc3))

(hear (mul (sin-osc ar (mouse-x kr 40 10000 1 0.1) 0) 0.1))

(hear
 (mce2 (mul (sin-osc ar (mouse-x kr 20 2000 1 0.1) 0)
            (mouse-y kr 0.01 0.1 0 0.1))
       (mul (sin-osc ar (mouse-y kr 20 2000 1 0.1) 0)
            (mouse-x kr 0.01 0.1 0 0.1))))

;; Auto-pilot variant
(hear (mul (sin-osc ar (mouse-x* kr 40 10000 1 0.1) 0) 0.1))
