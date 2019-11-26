;; seqr (rd)

(define nfreq
  (lambda (n l r)
    (map midi-cps (replicate-m n (i-random l r)))))

(define seqr-f
  (lambda (f e)
    (let ((n (/ (length e) 2)))
      (select (mul-add (lf-saw kr f 0) n n) (make-mce e)))))

;; (show-graph (seqr))
(define seqr
  (lambda ()
    (let* ((n (i-random 6 18))
           (f (/ (i-random 9 18) n)))
      (mul (blip ar
                 (mce2 (seqr-f f (nfreq n 72 96))
                       (seqr-f f (nfreq n 72 84)))
                 (mce2 (seqr-f f (replicate-m n (random 1 3)))
                       (seqr-f f (replicate-m n (random 3 6)))))
           (mce2 (seqr-f f (replicate-m n (random 0.05 0.10)))
                 (seqr-f f (replicate-m n (random 0.05 0.15))))))))

(with-sc3 (overlap-texture (list 6 6 3 +inf.0) seqr))
