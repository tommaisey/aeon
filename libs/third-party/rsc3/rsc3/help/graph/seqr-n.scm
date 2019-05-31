;; seqr-n (rd)

(import (rsc3) (rsc3 lang))

(define seqr-f
  (lambda (f e)
    (let ((n (/ (length (mce-channels e)) 2)))
      (select (mul-add (lf-saw kr f 0) n n) e))))

(define seqr-n
  (lambda (n)
    (let ((f (fdiv (i-rand 9 18) n)))
      (mul (blip ar
                 (mce2 (seqr-f f (midi-cps (i-rand-n n 72 96)))
                       (seqr-f f (midi-cps (i-rand-n n 72 84))))
                 (mce2 (seqr-f f (rand-n n 1 3))
                       (seqr-f f (rand-n n 3 6))))
           (mce2 (seqr-f f (rand-n n 0.05 0.10))
                 (seqr-f f (rand-n n 0.05 0.15)))))))

(with-sc3 (overlap-texture-u (list 6 6 3 +inf.0) (seqr-n 13)))
