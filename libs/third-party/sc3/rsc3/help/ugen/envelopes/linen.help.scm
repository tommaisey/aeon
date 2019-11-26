;; (linen gate attackTime susLevel releaseTime doneAction)

;; A linear envelope generator.  The done flag is set when the
;; envelope reaches zero.

;; Note that the sustain level input is consulted only at the instant
;; when the gate is opened.

(let ((e (linen (impulse kr 2 0) 0.01 0.1 0.4 do-nothing)))
  (audition (out 0 (mul (sin-osc ar 440 0) e))))

(let* ((y (mouse-y kr 0.1 0.5 0 0.1))
       (x (mouse-x kr -1 1 0 0.1))
       (e (linen x 1 y 1.0 do-nothing))
       (o (sin-osc ar 440 0)))
  (audition (out 0 (mul o e))))

;; Open gate for a random interval.

(let* ((r (rand 0.05 0.4))
       (u (letc ((gate 0))
	    (let ((e (linen gate 0.1 0.2 0.1 do-nothing)))
	      (out 0 (mul (sin-osc ar 440 0) e)))))
       (g (encode-graphdef (synthdef "linen" u))))
  (with-sc3
   (lambda (fd)
     (async fd (d-recv g))
     (send fd (s-new0 "linen" 1001 1 1))
     (send fd (bundle (utc) (n-set1 1001 "gate" 1)))
     (send fd (bundle (+ (utc) r)
		    (n-set1 1001 "gate" 0)))
     (sleep (* r 4))
     (send fd (n-free1 1001)))))
