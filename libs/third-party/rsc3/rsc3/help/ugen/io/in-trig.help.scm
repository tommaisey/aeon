;; (in-trig num-channels bus)

;; Generate a trigger anytime a bus is set.

;; Any time the bus is "touched" ie. has its value set (using "/c_set"
;; etc.), a single impulse trigger will be generated.  Its amplitude
;; is the value that the bus was set to.

;; Run an oscillator with the trigger at bus 10.

(let* ((t (in-trig 1 10))
       (p (env-perc 0.01 1 1 (list -4 -4)))
       (e (env-gen kr t t 0 1 do-nothing p))
       (f (mul-add (latch t t) 440 880)))
  (audition (out 0 (mul (sin-osc ar f 0) e))))

;; Set bus 10.

(with-sc3 
 (lambda (fd) 
   (send fd (c-set1 10 0.5))))
