;; (play-buf numChannels bufnum rate trigger startPos loop doneAction)

(import (rnrs) (rsc3))

(with-sc3
 (lambda (fd)
   (async fd (b-alloc-read 10 "/home/rohan/data/audio/pf-c5.aif" 0 0))))

;; Play once only.

(audition (out 0 (play-buf 1 ar 10 (buf-rate-scale kr 10) 1 0 no-loop do-nothing)))

;; Play in infinite loop.

(audition (out 0 (play-buf 1 ar 10 (buf-rate-scale kr 10) 1 0 loop do-nothing)))

;; trigger playback at each pulse.

(audition (out 0 (play-buf 1 ar 10 (buf-rate-scale kr 10) (impulse kr 2 0) 0 no-loop do-nothing)))

;; trigger playback at each pulse (diminishing intervals).

(let ((t (impulse kr (x-line kr 0.1 100 10 remove-synth) 0)))
  (audition (out 0 (play-buf 1 ar 10 (buf-rate-scale kr 10) t 0 0 0))))

;; Loop playback, accelerating pitch.

(let ((rate (x-line kr 0.1 100 60 remove-synth)))
  (audition (out 0 (play-buf 1 ar 10 rate 1 0 1 0))))

;; Sine wave control of playback rate, negative rate plays backwards.

(let ((r (mul-add (f-sin-osc kr (x-line kr 0.2 8 30 remove-synth) 0) 3 0.6)))
  (audition (out 0 (play-buf 1 ar 10 (mul (buf-rate-scale kr 10) r) 1 0 1 0))))

;; Release buffer.

(with-sc3
 (lambda (fd)
   (async fd (b-free 10))))
