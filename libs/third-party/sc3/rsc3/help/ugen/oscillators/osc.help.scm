(with-sc3
 (lambda (fd)
   (async fd (b-alloc 10 512 1))
   (async fd (b-gen1 10 "sine1" (list (+ 1 2 4) 1 1/2 1/3 1/4 1/5)))))

(audition (out 0 (mul (osc ar 10 220 0) 0.1)))

;; Modulate freq

(let ((f (x-line kr 2000 200 1 remove-synth)))
  (audition (out 0 (mul (osc ar 10 f 0) 0.5))))

;; Modulate freq

(let* ((f1 (x-line kr 1 1000 9 remove-synth))
       (f2 (mul-add (osc ar 10 f1 0) 200 800)))
  (audition (out 0 (mul (osc ar 10 f2 0) 0.25))))

;; Modulate phase

(let* ((f (x-line kr 20 8000 10 remove-synth))
       (p (mul (osc ar 10 f 0) (* 2 pi))))
  (audition (out 0 (mul (osc ar 10 800 p) 0.25))))

;; Change the buffer while its playing

(audition (out 0 (mul (osc ar 10 220 0) 0.1)))

(with-sc3
 (lambda (fd)
   (async fd (b-gen1 10 "sine1" (list (+ 1 2 4) 1 (random 0 1) 1/4)))))
