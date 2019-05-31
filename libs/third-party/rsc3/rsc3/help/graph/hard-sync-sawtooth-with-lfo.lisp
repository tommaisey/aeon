;; hard sync sawtooth with lfo (jmcc) #6

(define hsswl
  (let* ((f (midi-cps (add 30 (i-rand 0 50))))
         (o (mul-add (sin-osc kr 0.2 (mce2 0 (rand 0 (* 2 pi)))) (mul 2 f) (mul 3 f))))
    (mul (sync-saw ar (mce2 f (add f 0.2)) o) 0.05)))

(define hsswl-pp
  (lambda (z)
    (add (comb-n z 0.3 0.3 4) (mce-reverse z))))

(with-sc3*
 (list
  (post-process-u 2 hsswl-pp)
  (overlap-texture-u (list 4 4 4 +inf.0) hsswl)))
