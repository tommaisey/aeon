;; reso-pulse (jmcc) #1

(define reso-pulse
  (let* ((f (midi-cps (l-choose (list 25 30 34 37 41 42 46 49 53 54 58 61 63 66))))
         (f* (mul-add 2 f (rand2 0.5))))
    (mul (add (lf-pulse ar f 0 0.2) (lf-pulse ar f* 0 0.2)) 0.02)))

(define reso-pulse-pp
  (lambda (z)
    (let* ((lfo-freq 6)
           (lfo (mul-add (lf-noise0 kr lfo-freq) 1000 1200))
           (x (mouse-x kr 0.2 0.02 exponential 0.2))
           (left (rlpf z lfo x))
           (delay-time (fdiv 2 lfo-freq))
           (right (delay-n left delay-time delay-time)))
      (mce2 left right))))

(with-sc3*
 (list
  (post-process-u 1 reso-pulse-pp)
  (overlap-texture-u (list 4 2 4 +inf.0) reso-pulse)))
