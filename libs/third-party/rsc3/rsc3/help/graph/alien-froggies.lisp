;; alien froggies (jmcc) #1

(define alien-froggies
  (lambda (r)
    (let* ((r* (fold (mul r (u:exp (lin-rand -0.2 0.2 0))) 1 30))
           (o (formant ar r* (exp-rand 200 3000) (mul-add (rand 0 9) r* r*))))
      (mul o 0.05))))

(with-sc3 (overlap-texture-u (list 0.25 0.5 5 +inf.0) (alien-froggies 11)))
