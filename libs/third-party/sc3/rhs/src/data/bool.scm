;; otherwise :: Bool
(define otherwise #t)

;; not :: Bool -> Bool
(define not (lambda (x) (if (equal? x #f) #t #f)))
