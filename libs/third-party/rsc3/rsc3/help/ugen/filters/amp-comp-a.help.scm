;; (amp-compA freq root minAmp rootAmp)

;; ANSI A-weighting curve.

;; Basic psychoacoustic amplitude compensation

;; Higher frequencies are normally perceived as louder, which amp-compA
;; compensates. Following the measurings by Fletcher and Munson, the
;; ANSI standard describes a function for loudness vs. frequency.
;; Note that this curve is only valid for standardized amplitude.
;; For a simpler but more flexible curve, see amp-comp.

;; freq - input frequency value. For freq == root, the output is
;;        rootAmp. (default freq 0 Hz)

;; root - root freq relative to which the curve is calculated (usually
;;        lowest freq) (default 0 Hz) default value: C (60.midicps)

;; minAmp - amplitude at the minimum point of the curve (around 2512
;;          Hz) (default -10dB)

;; rootAmp - amplitude at the root frequency. (default 1) apart from
;;           freq, the values are not modulatable

;; compare a sine without compensation with one that uses amplitude
;; compensation

(let* ((x (mouse-x kr 300 15000 1 0.1))
       (y (mouse-y kr 0 1 0 0.1))
       (o (mul (sin-osc ar x 0) 0.1))
       (c (amp-comp-a x 300 (db-amp -10) 1)))
  (audition (out 0 (mce2 (mul o y) (mul3 o (sub 1 y) c)))))

;; adjust the minimum and root amp (in this way one can flatten out
;; the curve for higher amplitudes)

(let* ((x (mouse-x kr 300 18000 1 0.1))
       (y (mouse-y kr 0 1 0 0.1))
       (o (mul (formant ar 300 x 20) 0.1))
       (c (amp-comp-a x 300 0.6 0.3)))
  (audition (out 0 (mce2 (mul o y) (mul3 o (sub 1 y) c)))))

;; amplitude compensation in frequency modulation (using
;; Fletscher-Munson curve)

(let* ((x (mouse-x kr 300 15000 1 0.1))
       (y (mouse-y kr 3 200 1 0.1))
       (m (mul x (mul-add (sin-osc ar y 0) 0.5 1)))
       (a (amp-comp-a m 300 (db-amp -10) 1))
       (c (mul3 (sin-osc ar m 0) 0.1 a)))
  (audition (out 0 c)))
