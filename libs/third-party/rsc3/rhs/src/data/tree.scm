;; Tree a -> [a] -> [a]
(define flatten*
  (lambda (t r)
    (cond ((null? t) r)
          ((pair? t) (flatten* (head t) (flatten* (tail t) r)))
          (else (cons t r)))))

;; Tree a -> [a]
(define flatten (lambda (t) (flatten* t nil)))

;; Tree a -> [[a]]
(define levels
  (lambda (t)
    (if (null? t)
	nil
	(let ((lr (partition* (compose not pair?) t)))
	  (cons (car lr) (levels (concat (cdr lr))))))))
