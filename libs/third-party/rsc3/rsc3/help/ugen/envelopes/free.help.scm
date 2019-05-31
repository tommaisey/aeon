;; (free trig nodeID)

;; When triggered frees a node.

;; trig   - when triggered, frees node
;; nodeID - node to be freed

(with-sc3
 (lambda (fd)
   (send-synth fd "a" (out 0 (mul (sin-osc ar 800 0) 0.1)))
   (send-synth fd "b" (mrg2 (out 1 (mul (pink-noise ar) 0.1)) 
			   (free (dust ar 6) 1001)))
   (send fd (s-new0 "a" 1001 0 0))
   (send fd (s-new0 "b" -1 0 0))))

(with-sc3 reset)
