;; narrow band filtered crackle noise (jmcc) #2

(define nbfcn
  (let* ((e (env-linen 2 5 2 1 (list)))
         (rf1 (add (rand 0 2000) 80))
         (rf2 (mul-add (rand2 0.2) rf1 rf1))
         (rf (x-line kr rf1 rf2 9 do-nothing))
         (c (mul (add (crackle ar 1.97) (rand 0 0.03)) 0.15)))
    (pan2 (resonz c rf 0.2) (rand2 1) (env-gen ar 1 1 0 1 remove-synth e))))

(with-sc3 (spawn-u (list 2 +inf.0) nbfcn))
