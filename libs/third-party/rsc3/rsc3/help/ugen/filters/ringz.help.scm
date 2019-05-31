(hear (ringz (mul (dust ar 3) 0.3) 2000 2))

(hear (ringz (mul (white-noise ar) 0.005) 2000 0.5))

;; Modulate frequency

(hear
 (ringz (mul (white-noise ar) 0.005)
        (x-line kr 100 3000 10 do-nothing)
        0.5))

(hear
 (ringz (mul (impulse ar 6 0) 0.3)
        (x-line kr 100 3000 10 do-nothing)
        0.5))

;; Modulate ring time

(hear
 (ringz (mul (impulse ar 6 0) 0.3)
        2000
        (x-line kr 4 0.04 8 do-nothing)))

;; Modulate ring time opposite direction

(hear
 (ringz (mul (impulse ar 6 0) 0.3)
        2000
        (x-line kr 0.04 4 8 do-nothing)))

(hear
 (let ((n (mul (white-noise ar) 0.001)))
   (mix-fill
    10
    (lambda (_)
      (let ((f (x-line kr (rand 100 5000) (rand 100 5000) 20 do-nothing)))
        (ringz n f 0.5))))))
