(audition (out 0 (resonz (mul (white-noise ar) 0.5) 2000 0.1)))

;; Modulate frequency

(let ((f (x-line kr 1000 8000 10 remove-synth)))
  (audition (out 0 (resonz (mul (white-noise ar) 0.5) f 0.05))))

;; Modulate bandwidth

(let ((rq (x-line kr 1 0.001 8 remove-synth)))
  (audition (out 0 (resonz (mul (white-noise ar) 0.5) 2000 rq))))

;; Modulate bandwidth opposite direction

(let ((rq (x-line kr 0.001 1 8 remove-synth)))
  (audition (out 0 (resonz (mul (white-noise ar) 0.5) 2000 rq))))

;; random resonator at a random location, run as often as you like...

(let ((freq (choose (map (lambda (z) (* z 120)) (enum-from-to 1 16))))
      (bw 1/4)
      (gain 8))
  (audition (out 0 (pan2 (resonz (white-noise ar) freq (/ bw freq))
			 (rand -1 1)
			 gain))))
