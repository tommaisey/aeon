;; (slope in)

;; slope of signal.  Measures the rate of change per second of a
;; signal.  Formula implemented is:

;; out[i] = (in[i] - in[i-1]) * sampling_rate

;; in - input signal to measure.

;; a = quadratic noise, b = first derivative line segments, c = second
;; derivative constant segments

(let* ((r 2)
       (a (lf-noise2 kr r))
       (scale (recip r))
       (b (mul (slope a) scale))
       (c (mul (slope b) (squared scale)))
       (o (sin-osc ar (mul-add (mce3 a b c) 220 220) 0)))
  (audition (out 0 (mix (mul o 1/3)))))
