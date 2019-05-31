;; resonators harmonic series (jmcc) #2

(define resonators-harmonic-series
  (let* ((p 12)
         (noise (mul (brown-noise ar) 0.001))
         (rat (list 1.0 1.125 1.25 1.333 1.5 1.667 1.875 2.0 2.25 2.5 2.667 3.0 3.333 3.75 4.0))
         (freq (mul (l-choose rat) 120))
         (res-freqs (zip-with add ((series* add) p freq freq) (replicate-m p (rand2 0.5))))
         (spec (klank-data
                res-freqs
                (map (lambda (i) (fdiv 1 (add i 1))) (enum-from-to 0 (- p 1)))
                (replicate-m p (rand 0.5 4.5)))))
    (clone 2 (klank noise 1 0 1 spec))))

(with-sc3 (xfade-texture-u (list 1 7 +inf.0) resonators-harmonic-series))
