;; (pause gate nodeID)

;; When triggered pauses a node.

;; gate   - when gate is 0,  node is paused, when 1 it runs
;; nodeID - node to be paused

(with-sc3
 (lambda (fd)
   (send-synth fd "a" (out 0 (mul (sin-osc ar 800 0) 0.1)))
   (send-synth fd "b" (letc ((g 1))
			(Mrg (out 1 (mul (pink-noise ar) 0.05))
			     (pause g 1001))))
   (send fd (/s_new "a" 1001 0 0))
   (send fd (/s_new "b" 1002 0 0))
   (sleep 1)
   (send fd (/n_set 1002 "g" 0))
   (sleep 1)
   (send fd (/n_set 1002 "g" 1))
   (sleep 1)
   (reset fd)))
