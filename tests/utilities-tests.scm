;; Test the utility function library.
(module utilities-tests ()

  (import (chezscheme)
          (testing)
          (utilities))

  (test-eq "[utilities identity]" 10 (identity 10))
  (test-eq "[utilities identity]" 'hi (identity 'hi))
  
  (test-eq "[utilities compose]" #t ((compose inc zero?) -1))
  (test-eq "[utilities compose]" #f ((compose dec (lambda (x) (> x 5))) 5))

  (test-eq "[utilities trunc-int]" 2 (trunc-int 2.8))
  (test-eq "[utilities trunc-int]" -2 (trunc-int -2.8))
  (test-eq "[utilities trunc-int]" 1 (trunc-int 9/7))

  (test-eq "[utilities round-down]" 8 (round-down 9.8 4))
  (test-eq "[utilities round-down]" -4 (round-down -2.8 2))
  (test-eq "[utilities round-down]" 0 (round-down 5/4 2))

  (test-eq "[utilities round-up]" 12 (round-up 9.8 4))
  (test-eq "[utilities round-up]" -2 (round-up -2.8 2))
  (test-eq "[utilities round-up]" 2 (round-up 5/4 2))

  (test-eq "[utilities round-nearest]" 8 (round-nearest 9.8 4))
  (test-eq "[utilities round-nearest]" -4 (round-nearest -3.3 2))
  (test-eq "[utilities round-nearest]" 2 (round-nearest 5/4 2))

  (test-eq "[utilities inc]" 10 (inc 9))
  (test-eq "[utilities inc]" -8 (inc -9))
  (test-eq "[utilities dec]" 8 (dec 9))
  (test-eq "[utilities dec]" -10 (dec -9))

  (test-eq "[utilities clamp]" 3 (clamp 3 1 4))
  (test-eq "[utilities clamp]" 4 (clamp 5 1 4))
  (test-eq "[utilities clamp]" 1 (clamp -1 1 4))
  (test-eq "[utilities clamp]" -3 (clamp -2 -5 -3))

  (test-eq "[utilities between?]" #t (between? 3 1 4))
  (test-eq "[utilities between?]" #t (between? 1 1 4))
  (test-eq "[utilities between?]" #f (between? 4 1 4))
  (test-eq "[utilities between?]" #f (between? 0 1 4))
  (test-eq "[utilities between?]" #f (between? 5 1 4))

  (test-eq "[utilities between-inclusive?]" #t (between-inclusive? 3 1 4))
  (test-eq "[utilities between-inclusive?]" #t (between-inclusive? 1 1 4))
  (test-eq "[utilities between-inclusive?]" #t (between-inclusive? 4 1 4))
  (test-eq "[utilities between-inclusive?]" #f (between-inclusive? 0 1 4))
  (test-eq "[utilities between-inclusive?]" #f (between-inclusive? 5 1 4))

  (test-eq "[utilities pi]" #t (between-inclusive? pi 3.141592 3.141593))
  
  (test-eq "[utilities range-sine]" 2.0 (range-sine 2 1 3 0))
  (test-eq "[utilities range-sine]" 3.0 (range-sine 2 1 3 0.5))
  (test-eq "[utilities range-sine]" 2.0 (range-sine 2 1 3 1))
  (test-eq~ "[utilities range-sine]" (* pi 0.5) (range-sine 1 0 pi 0.5))

  (test-eq "[utilities pair]" pair cons)
  (test-eq "[utilities first]" first car)
  (test-eq "[utilities rest]" rest cdr)
  (test-eq "[utilities cons-r]" '(2 . 1) (cons-r 1 2))

  (test-eq "[utilities sorted?]" #t (sorted? < (list 7 8 9)))
  (test-eq "[utilities sorted?]" #f (sorted? < (list 7 9 8)))
  (test-eq "[utilities sorted?]" #t (sorted? > (list 9 8 7)))

  (test-eq "[utilities merge-sorted]"
    (list 1 2 5 7 8 11)
    (merge-sorted < (list 1 5 8) (list 2 7 11)))

  (test-eq "[utilities pairwise]"
    #f (pairwise (list 4 5 9)))
  (test-eq "[utilities pairwise]"
    '((4 . 5) (9 . 7) (2 . 2))
    (pairwise (list 4 5 9 7 2 2)))
  (test-eq "[utilities repeat]"
    '(5 5 5 5) (repeat 4 5))
  (test-eq "[utilities repeat]"
    '() (repeat -2 5))
  (test-eq "[utilities flatten]"
    '(1 2 3 4 5 6)
    (flatten (list 1 2 (list 3 4 '(5 . 6)))))
  (test-eq "[utilities take]"
    '(1 2 3) (take '(1 2 3 4 5 6) 3))
  (test-eq "[utilities take]"
    '(1 2) (take '(1 2) 3))
  (test-eq "[utilities take]"
    '() (take '() 3))

  (test-eq "[utilities extend-repeating-last]"
    '(1 3 3 3 3) (extend-repeating-last '(1 3) 5))

  (test-eq "[utilities for-any]"
    #t (for-any zero? (list 1 2 3 0)))
  (test-eq "[utilities for-any]"
    #f (for-any zero? (list 1 2 3)))
  (test-eq "[utilities for-none]"
    #f (for-none zero? (list 1 2 3 0)))
  (test-eq "[utilities for-none]"
    #t (for-none zero? (list 1 2 3)))

  (test-eq "[utilities list-index]"
    2 (list-index zero? (list 1 2 0 3)))
  (test-eq "[utilities list-last]"
    3 (list-last (list 1 2 0 3)))
  
  (test-eq "[utilities unsafe-list]"
    #t (unsafe-list? (list)))
  (test-eq "[utilities unsafe-list]"
    #f (unsafe-list? 3))
  (test-eq "[utilities unsafe-list]"
    #f (unsafe-list? (cons 2 3)))
  (test-eq "[utilities unsafe-list]"
    #t (unsafe-list? (cons 2 (cons 3 4)))) ; the 'unsafe' case.

  (test-eq "[utilities push-front]"
    '(1 2 . 3) (push-front 1 (cons 2 3)))
  (test-eq "[utilities push-front]"
    '(1 2 3) (push-front 1 (list 2 3)))
  (test-eq "[utilities push-front]"
    '(0 1 2 3) (push-front (list 0 1) (list 2 3)))

  (test-eq "[utilities delete-duplicates]"
    '(3 2 7 1) (delete-duplicates (list 3 2 3 7 2 2 1)))
  (test-eq "[utilities lset-difference]"
    '(3 8) (lset-difference eqv? (list 3 6 7 8) (list 6 7)))

  (test-eq "[utilities find-first-slot]"
    #f (find-first-slot (vector #f #f #f)))
  (test-eq "[utilities find-first-slot]"
    2 (find-first-slot (vector #f #f 'hi #f)))
  (test-eq "[utilities find-first-slot]"
    1 (find-first-slot (vector 'hi 'bye 'hi 'hi) (lambda (x) (eqv? x 'bye))))

  (let ([alist-example '((a . 42) (b . 31))])
    (test-eq "[utilities make-alist]"
      alist-example (make-alist 'a 42 'b 31))
    (test-eq "[utilities alist-get]"
      31 (alist-get alist-example 'b 99))
    (test-eq "[utilities alist-set]"
      '((c . 99) (a . 42) (b . 31)) (alist-set alist-example 'c 99))
    (test-eq "[utilities alist-set-if-not]"
      alist-example (alist-set-if-not alist-example 'b 99))
    (test-eq "[utilities alist-get-multi]"
      '((z . 3) (x . 1) (q . 7))
      (alist-get-multi '((x . 1) (y .2) (z . 3))
                       (make-alist 'x #f 'z #f 'q 7))))
  
  )
