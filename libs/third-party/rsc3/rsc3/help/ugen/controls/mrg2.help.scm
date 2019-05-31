;; (mrg2 left right)

;; mrg2 defines a node indicating a multiple root graph.

(audition
 (let ((l (out 0 (mul (sin-osc ar 300 0) 0.1)))
       (r (out 1 (mul (sin-osc ar 900 0) 0.1))))
   (mrg2 l r)))

;; there is a leftmost rule, so that mrg nodes need not
;; be terminal.

(hear
 (let ((l (mul (sin-osc ar 300 0) 0.1))
       (r (out 1 (mul (sin-osc ar 900 0) 0.1))))
  (mrg2 l r)))

;; the leftmost node may be an mce node

(hear
 (let ((l (mul (sin-osc ar (mce2 300 400) 0) 0.1))
       (r (out 1 (mul (sin-osc ar 900 0) 0.1))))
   (mrg2 l r)))

;; the implementation is not thorough

(hear
 (let ((l (mul (sin-osc ar (mce2 300 400) 0) 0.1))
       (r (out 1 (mul (sin-osc ar 900 0) 0.1))))
   (add (mrg2 l r) (mrg2 l r))))
