;; record scratcher (josh parmenter)

(define dup (lambda (a) (mce2 a a)))

(define record-scratcher
  (lambda (b)
    (let* ((x (mouse-x kr -10 10 linear 0.2))
           (dx (sub x (delay-n x 0.1 0.1)))
           (bdx (add (mouse-button kr 1 0 0.3) dx))
           (bdxr (mul bdx (buf-rate-scale kr b))))
      (dup (play-buf 1 ar b bdxr 0 0 loop do-nothing)))))

(with-sc3
 (lambda (fd)
   (begin
     (async fd (b-alloc-read 10 "/home/rohan/data/audio/pf-c5.snd" 0 0))
     (play fd (out 0 (record-scratcher 10))))))
