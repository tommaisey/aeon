;; bit reduction (adc)

(define down-sample
  (let* ((f (lf-noise2 kr 8))
         (nh (lf-noise2 kr 3))
         (src (blip ar (mul-add f 200 300) (mul-add nh 10 20)))
         (sr (mouse-x kr 1000 (mul sample-rate 0.1) 1 0.2)))
    (latch src (impulse ar sr 0))))

;; (hear down-sample)

(define bit-reduction
  (let* ((bit-sz (mouse-y kr 1 24 1 0.2))
         (bit-redux (u:round down-sample (pow 0.5 bit-sz))))
    (mce2 down-sample bit-redux)))

(hear bit-reduction)

