;; (amp-comp freq root exp)

;; Basic psychoacoustic amplitude compensation.

;; Implements the (optimized) formula: compensationFactor = (root /
;; freq) ** exp.  Higher frequencies are normally perceived as louder,
;; which amp-comp compensates.

;; See also amp-compA

;; freq - input frequency value. For freq == root, the output is 1.0.

;; root - root freq relative to which the curve is calculated (usually
;;        lowest freq) default value: C (60.midicps)

;; exp - exponent. how steep the curve decreases for increasing freq
;;       (see plots below). default value 0.3333

;; Note that for frequencies very much smaller than root the
;; amplitudes can become very high.  in this case limit the freq with
;; freq.max(minval), or use amp-compA.

;; compare a sine without compensation with one that uses amplitude
;; compensation

(let* ((x (mouse-x kr 300 15000 1 0.1))
       (y (mouse-y kr 0 1 0 0.1))
       (o (mul (sin-osc ar x 0) 0.1))
       (c (amp-comp x 300 0.333))) 
  (audition (out 0 (mce2 (mul o y) (mul3 o (sub 1 y) c)))))

;; different sounds cause quite different loudness perception, and the
;; desired musical behavior can vary, so the exponent can be tuned:

(let* ((x (mouse-x kr 300 15000 1 0.1))
       (o (mul (pulse ar x 0.5) 0.1))
       (c (amp-comp x 300 1.3)))
  (audition (out 0 (mul o c))))

;; amplitude compensation in frequency modulation

(let* ((x (mouse-x kr 300 15000 1 0.1))
       (y (mouse-y kr 3 200 1 0.1))
       (m (mul x (mul-add (sin-osc ar y 0) 0.5 1)))
       (a (amp-comp m 300 0.333))
       (c (mul3 (sin-osc ar m 0) 0.1 a)))
  (audition (out 0 c)))
