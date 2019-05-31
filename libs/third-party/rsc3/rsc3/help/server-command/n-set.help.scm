; /n_set                                Set a node's control value(s)

; int - node ID
; [
; int or string - a control index or name
; float - a control value
; ] * N

; Takes a list of pairs of control indices and values and sets the
; controls to those values. If the node is a group, then it sets the
; controls of every node in the group.

(with-sc3
 (lambda (fd)
   (letc ((f 440)
          (a 0.1))
     (send-synth fd "sin" (out 0 (mul (sin-osc ar f 0) a))))
   (send fd (s-new0 "sin" 1001 add-to-tail 1))))

(with-sc3
 (lambda (fd)
   (send fd (n-set1 1001 "f" 1280))))

(with-sc3
 (lambda (fd)
   (send fd (n-set 1001 (list (tuple2 "f" (random 60 900))
                              (tuple2 "a" (random 0.05 0.25)))))))
