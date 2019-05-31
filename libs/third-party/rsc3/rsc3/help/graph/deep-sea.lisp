;; deep sea (jrhb)

(define range
  (lambda (s l r)
    (let ((m (mul (sub r l) 0.5)))
      (mul-add s m (add m l)))))

(define deep-sea
  (let* ((amp 1)
         (pan 0)
         (variation 0.9)
         (n (rand 7 46))
         (dt1 (add 25 (rand -1.7 1.7)))
         (dt2 (mul3 (add dt1 (lf-noise2 kr 2)) variation 0.001))
         (freq (add 901 (rand 0 65)))
         (t (mul (impulse ar (recip dt2) 0) 100))
         (count (pulse-count t 0))
         (ml (u:lt count n))
         (u1 (mul (bpf (mul ml t) freq 1) 0.1))
         (freq2 (mul freq
                     (add (u:mod count (range (lf-noise1 kr 1) 2 20)) 1)))
         (u2 (mul (bpf u1 freq2 1) 0.2)))
    (mrg2 (pan2 u2 pan (mul amp 10))
          (detect-silence u2 0.0001 0.2 remove-synth))))

(hear deep-sea)
