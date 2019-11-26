;; bouncing objects (jmcc) #2

(define bouncing-objects
  (let* ((imp-frq (x-line kr (add 5 (rand -2 2)) 600 4 do-nothing))
         (imp-amp (x-line kr 0.09 0.000009 4 do-nothing))
         (imp (mul (impulse ar imp-frq 0) imp-amp))
         (exc (decay imp 0.001))
         (flt-frq (clone 4 (rand 400 8400)))
         (flt-amp (clone 4 (rand 0 1)))
         (flt-rtm (clone 4 (rand 0.01 0.11)))
         (flt (klank exc 1 0 1 (klank-data-mce flt-frq flt-amp flt-rtm)))
         (loc (pan2 flt (rand -1 1) 1))
         (e (env '(1 1 0) '(3 0.001) (replicate 2 'linear) -1 -1)))
    (mul loc (env-gen kr 1 1 0 1 remove-synth e))))

(with-sc3
 (spawn-u (list 0.6 +inf.0) (delay-n bouncing-objects 1 (rand 0 1))))
