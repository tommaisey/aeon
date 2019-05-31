;; swept resonant noise (jmcc) #2

(define srn
  (let* ((p 10)
         (n (mul (white-noise ar) 0.007))
         (f (midi-cps (mul-add (f-sin-osc kr (rand 0.1 0.3) 0) (rand 0 24) (rand 36 84))))
         (sweep (resonz n f 0.1))
         (spec-f (lambda ()
                   (klank-data-mce (lin-rand-n p 80 10080 0)
                                   (make-mce (replicate p 1))
                                   (rand-n p 0.5 2.5))))
         (spec (replicate-m 2 spec-f)))
    (make-mce (replicate-m 2 (klank sweep 1 0 1 (spec-f))))))

(with-sc3 (overlap-texture-u (list 4 4 5 +inf.0) srn))
