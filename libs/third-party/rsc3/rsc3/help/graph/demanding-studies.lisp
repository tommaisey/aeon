;; demanding studies (jmcc)

(define demanding-studies
  (let* ((s1 (drand dinf (mce4 72 75 79 82)))
         (s2 (drand 1 (mce3 82 84 86)))
         (s3 (dseq dinf (mce4 72 75 79 s2)))
         (x (mouse-x kr 5 6 0 0.2))
         (tr (impulse kr x 0))
         (f (demand tr 0 (mce2 (midi-cps (sub s1 12)) (midi-cps s3))))
         (o1 (sin-osc ar (add f (mce2 0 0.7)) 0))
         (o2 (mul (saw ar (add f (mce2 0 0.7))) 0.3))
         (o3 (cubed (distort (u:log (distort (add o1 o2)))))))
    (mul o3 0.1)))

(hear demanding-studies)
