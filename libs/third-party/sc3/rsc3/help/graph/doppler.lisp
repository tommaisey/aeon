;; doppler (jmcc)/(rd)

(define doppler
  (lambda (xy s)
    (let* ((d (hypot (car xy) (cdr xy)))
           (a (fdiv 40 (squared d)))
           (maxd 110))
      (mul (delay-l s (fdiv maxd 344) (fdiv d 344)) a))))

(define line-x (lambda (x y dt) (cons x (line kr (neg y) y dt remove-synth))))

(define ellipse
  (lambda (a b dt)
    (let ((t (line kr (neg two-pi) (mul pi 1.5) dt remove-synth)))
      (cons (mul a (u:cos t)) (mul b (u:sin t))))))

(define src (rlpf (mul (f-sin-osc ar 200 0) (lf-pulse ar 31.3 0 0.4)) 400 0.3))

(define d1 (doppler (line-x 10 100 6) src))

(define d2 (doppler (ellipse 10 75 12) src))

(hear d1)
(hear d2)
