; Data.List

(list 1 2 3) ; (1 2 3)

(define l (cons 1 (cons 2 (cons 3 '())))) ; nil
l ; (1 2 3)
(null? l) ; #f
(pair? l) ; #t
(list? l) ; #t
(length l) ; 3
(append l l) ; (1 2 3 1 2 3)

(append '(1 2 3) (list 4 5 6)) ; (1 2 3 4 5 6)

(foldl + 0 (list 1 2 3)) ; 6
(foldl (flip cons) '() (list 1 2 3)) ; (3 2 1)

(foldr cons '() (list 1 2 3)) ; (1 2 3)
(list 1 2 3) ; (1 2 3)
(list 1 2 ((lambda (_) (cons 3 4)) nil) 5) ; (1 2 (cons 3 4) 5)

(maximum '(1 3 5 4 2 0)) ; 5
(minimum '(1 3 5 4 2 0)) ; 5

nil ; '()

(nub '(1 2 2 3 3 3)) ; (1 2 3)
(nub-by (on equal? car) '((0 1) (0 2) (1 2))) ; ((0 1) (1 2))

(not-elem 1 '(0 2 4))

(reverse (list 1 2 3)) ; (3 2 1)

; mapAccumL (\st x -> let r = st + x in (r,(x,r))) 0 [1,3,5,7,9]
; (25,[(1,1),(3,4),(5,9),(7,16),(9,25)])
(map-accum-l (lambda (st x) (let ((r (+ st x))) (cons r (cons x r)))) 0 '(1 3 5 7 9))

; mapAccumR (\st x -> let r = st + x in (r,(x,r))) 0 [1,3,5,7,9]
; (25,[(1,25),(3,24),(5,21),(7,16),(9,9)])
(map-accum-r (lambda (st x) (let ((r (+ st x))) (cons r (cons x r)))) 0 '(1 3 5 7 9))

; Data.Tree

(flatten '(1 2 (3 4 (5)) 6)) ; (1 2 3 4 5 6)
(levels '(1 2 (3 4 (5)) 6)) ; ((1 2 6) (3 4) (5))

; Prelude

(enum-from-difference-to > 0 1 9) ; (0 1 2 3 4 5 6 7 8 9)
(enum-from-difference-to < 0 -3 -9) ; (0 -3 -6 -9)
(enum-from-then-to 0 1 9) ; (0 1 2 3 4 5 6 7 8 9)
(enum-from-then-to 0 -3 -9) ; (0 -3 -6 -9)
(enum-from-to 0 9) ; (0 1 2 3 4 5 6 7 8 9)
(map signum '(-3 0 3)) ; (-1 0 1)
(succ 1) ; 2
(pred 2) ; 1

; Control.Monad

(replicate-m 4 (begin (display 'r) (newline))) ; r\nr\nr\nr\n
