;; moto rev (jmcc) #1

(define moto-rev
  (let* ((f (mul-add (sin-osc kr 0.2 0) 10 21))
         (s (lf-pulse ar f (mce2 0 0.1) 0.1)))
    (clip2 (rlpf s 100 0.1) 0.4)))

(hear moto-rev)
