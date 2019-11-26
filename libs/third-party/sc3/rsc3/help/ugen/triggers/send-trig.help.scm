;; (send-trig in id value)

;; On receiving a trigger (0 to non-zero transition), send a trigger
;; message from the server back to all registered clients.  Clients
;; register by sending a /notify message to the server.

;; input - the trigger

;; id - an integer that will be passed with the trigger message.  This
;;   	is useful if you have more than one send-trig in a SynthDef

;; value - a UGen or float that will be polled at the time of trigger,
;;         and its value passed with the trigger message

(let ((s (lf-noise0 kr 10)))
  (audition (mrg2 (send-trig s 0 s)
		  (out 0 (mul (sin-osc ar (mul-add s 200 500) 0) 0.1)))))

(with-sc3
 (lambda (fd)
   (async fd (notify 1))
   (thread-sleep 2.0)
   (let ((r (wait fd "/tr")))
     (send fd (notify 0)) ;; async here will recv a '/tr' not a '/done' message
     r)))
