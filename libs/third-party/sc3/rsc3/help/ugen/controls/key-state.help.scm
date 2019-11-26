;; The keycode 38 is the A key on my keyboard.  Under X the xev(1)
;; command is useful in determining your keyboard layout.

(hear (mul (sin-osc ar 800 0) (key-state kr 38 0 0.1 0.5)))
