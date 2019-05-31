;; all :: (a -> Bool) -> [a] -> Bool
(define all
  (lambda (f l)
    (if (null? l)
        #t
        (and (f (car l)) (all f (cdr l))))))

;; and :: [Bool] -> Bool
(define all-true
  (lambda (l)
    (if (null? l)
        #t
        (and (car l) (all-true (cdr l))))))

;; any :: (a -> Bool) -> [a] -> Bool
(define any
  (lambda (f l)
    (if (null? l)
        #f
        (or (f (car l)) (any f (cdr l))))))

;; (++) :: [a] -> [a] -> [a]
(define append
  (lambda (a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b)))))

;; (define ++ append)

;; break :: (a -> Bool) -> [a] -> ([a],[a])
(define break (lambda (p l) (span (compose not p) l)))

;; concat :: [[a]] -> [a]
(define concat (lambda (l) (foldr append nil l)))

;; concatMap :: (a -> [b]) -> [a] -> [b]
(define concat-map (lambda (f l) (concat (map f l))))

;; deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
(define delete-by
  (lambda (f x l)
    (if (null? l)
        nil
        (if (f x (car l))
            (cdr l)
            (cons (car l) (delete-by f x (cdr l)))))))

;; delete :: (Eq a) => a -> [a] -> [a]
(define delete (lambda (x l) (delete-by equal? x l)))

;; drop :: Int -> [a] -> [a]
(define drop
  (lambda (n l)
    (cond ((<= n 0) l)
          ((null? l) nil)
          (else (drop (- n 1) (cdr l))))))

;; dropWhile :: (a -> Bool) -> [a] -> [a]
(define drop-while
  (lambda (p l)
    (if (null? l)
        nil
        (if (p (car l))
            (drop-while p (cdr l))
            l))))

;; elem :: (Eq a) => a -> [a] -> Bool
(define elem (lambda (x l) (any (lambda (y) (equal? x y)) l)))

;; elemIndex :: Eq a => a -> [a] -> Maybe Int
(define elem-index (lambda (x l) (find-index (lambda (y) (equal? x y)) l)))

;; elemIndices :: Eq a => a -> [a] -> [Int]
(define elem-indices (lambda (x l) (find-indices (lambda (y) (equal? x y)) l)))

;; find :: (a -> Bool) -> [a] -> Maybe a
(define find
  (lambda (f l)
    (if (null? l)
        #f
        (if (f (car l))
            (car l)
            (find f (cdr l))))))

(define find-index*
  (lambda (f l n)
    (if (null? l)
        #f
        (if (f (car l))
            n
            (find-index* f (cdr l) (+ n 1))))))

;; findIndex :: (a -> Bool) -> [a] -> Maybe Int
(define find-index (lambda (f l) (find-index* f l 0)))

(define find-indices*
  (lambda (f l n)
    (if (null? l)
        nil
        (if (f (car l))
            (cons n (find-indices* f (cdr l) (+ n 1)))
            (find-indices* f (cdr l) (+ n 1))))))

;; findIndices :: (a -> Bool) -> [a] -> [Int]
(define find-indices (lambda (f l) (find-indices* f l 0)))

;; filter :: (a -> Bool) -> [a] -> [a]
(define filter
  (lambda (f l)
    (if (null? l)
        nil
        (let ((x (car l))
              (xs (cdr l)))
          (if (f x)
              (cons x (filter f xs))
              (filter f xs))))))

;; foldl :: (a -> b -> a) -> a -> [b] -> a
(define foldl
  (lambda (f z l)
    (if (null? l)
        z
        (foldl f (f z (car l)) (cdr l)))))

;; foldl1 :: (a -> a -> a) -> [a] -> a
(define foldl1 (lambda (f l) (foldl f (car l) (cdr l))))

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(define foldr
  (lambda (f z l)
    (if (null? l)
        z
        (f (car l) (foldr f z (cdr l))))))

;; foldr1 :: (a -> a -> a) -> [a] -> a
(define foldr1
  (lambda (f l)
    (if (null? (cdr l))
        (car l)
        (f (car l) (foldr1 f (cdr l))))))

;; groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
(define group-by
  (lambda (f l)
    (if (null? l)
        '()
        (let* ((x (car l))
               (yz (span (lambda (e) (f e x)) (cdr l))))
          (cons (cons x (car yz)) (group-by f (cdr yz)))))))

;; head :: [a] -> a
(define head car)

;; init :: [a] -> [a]
(define init
  (lambda (l)
    (let ((x (car l))
          (xs (cdr l)))
      (if (null? xs)
          nil
          (cons x (init xs))))))

;; insert :: Ord a => a -> [a] -> [a]
(define insert (lambda (e l) (insert-by compare e l)))

;; insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
(define insert-by
  (lambda (f x l)
    (if (null? l)
        (list1 x)
        (if (equal? (f x (car l)) 'gt)
            (cons (car l) (insert-by f x (cdr l)))
            (cons x l)))))

;; intercalate :: [a] -> [[a]] -> [a]
(define intercalate (lambda (e l) (concat (intersperse e l))))

;; intersperse :: a -> [a] -> [a]
(define intersperse
  (lambda (x l)
    (cond ((null? l) nil)
          ((null? (cdr l)) l)
          (else (cons (car l) (cons x (intersperse x (cdr l))))))))

;; isInfixOf :: (Eq a) => [a] -> [a] -> Bool
(define is-infix-of
  (lambda (p q)
    (cond ((null? p) #t)
          ((null? q) #f)
          (else (or (is-prefix-of p q)
                    (is-infix-of p (cdr q)))))))

;; isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
(define is-prefix-of
  (lambda (p q)
    (cond ((null? p) #t)
          ((null? q) #f)
          (else (and (equal? (car p) (car q))
                     (is-prefix-of (cdr p) (cdr q)))))))

;; isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
(define is-suffix-of (lambda (p q) (is-prefix-of (reverse p) (reverse q))))

;; iterate :: (a -> a) -> a -> [a]
;;
;; the scheme variant takes a length argument, scheme lists are not lazy.
;;
;; (equal? (iterate 8 (lambda (n) (* n 2)) 1) 256)
(define iterate
  (lambda (n f z)
    (if (equal? n 0)
        z
        (iterate (- n 1) f (f z)))))

;; last :: [a] -> a
(define last
  (lambda (l)
    (let ((xs (cdr l)))
      (if (null? xs)
          (car l)
          (last xs)))))

;; length :: [a] -> Int
(define length
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (length (cdr l))))))

;; listn :: a ... -> [a]
(define list1 (lambda (x) (cons x nil)))
(define list2 (lambda (x y) (cons x (cons y nil))))
(define list3 (lambda (x y z) (cons x (cons y (cons z nil)))))
(define list4 (lambda (x y z a) (cons x (cons y (cons z (cons a nil))))))
(define list5 (lambda (x y z a b) (cons x (cons y (cons z (cons a (cons b nil)))))))

;; (!!) :: [a] -> Int -> a
(define list-ref
  (lambda (l n)
    (if (= n 0)
        (car l)
        (list-ref (cdr l) (- n 1)))))

(define !! list-ref)

;; lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
(define lookup
  (lambda (x l)
    (if (null? l)
        #f
        (if (equal? (car (car l)) x)
            (cdr (car l))
            (lookup x (cdr l))))))

;; map :: (a -> b) -> [a] -> [b]
(define map
  (lambda (f l)
    (if (null? l)
        nil
        (cons (f (car l)) (map f (cdr l))))))

;; mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
(define map-accum-l
  (lambda (f s l)
    (if (null? l)
        (cons s nil)
        (let* ((a (f s (car l)))
               (b (map-accum-l f (car a) (cdr l))))
          (cons (car a) (cons (cdr a) (cdr b)))))))

;; mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
(define map-accum-r
  (lambda (f s l)
    (if (null? l)
        (cons s nil)
        (let* ((a (map-accum-r f s (cdr l)))
               (b (f (car a) (car l))))
          (cons (car b) (cons (cdr b) (cdr a)))))))

;; maximum :: (Ord a) => [a] -> a
(define maximum (lambda (l) (foldl1 max l)))

;; minimum :: (Ord a) => [a] -> a
(define minimum (lambda (l) (foldl1 min l)))

;; nub :: (Eq a) => [a] -> [a]
(define nub (lambda (l) (nub-by equal? l)))

;; nubBy :: (a -> a -> Bool) -> [a] -> [a]
(define nub-by
  (lambda (f l)
    (if (null? l)
        nil
        (let ((x (car l))
              (xs (cdr l)))
          (cons x (nub-by f (filter (lambda (y) (not (f x y))) xs)))))))

;; nil :: [a]
(define nil '())

;; notElem :: (Eq a) => a -> [a] -> Bool
(define not-elem
  (lambda (x l)
    (all (lambda (y) (not (equal? x y))) l)))

;; null :: [a] -> Bool
(define null? (lambda (x) (equal? x nil)))

;; or :: [Bool] -> Bool
(define any-true
  (lambda (l)
    (if (null? l)
        #f
        (or (car l) (any-true (cdr l))))))

;; partition :: (a -> Bool) -> [a] -> ([a], [a])
(define partition*
  (let ((select (lambda (p)
                  (lambda (x tf)
                    (let ((t (car tf))
                          (f (cdr tf)))
                      (if (p x)
                          (cons (cons x t) f)
                          (cons t (cons x f))))))))
    (lambda (p xs)
      (foldr (select p) (cons nil nil) xs))))

;; product :: (Num a) => [a] -> a
(define product (lambda (l) (foldl * 1 l)))

;; replicate :: Int -> a -> [a]
(define replicate
  (lambda (n x)
    (if (= n 0)
        nil
        (cons x (replicate (- n 1) x)))))

;; reverse :: [a] -> [a]
(define reverse (lambda (l) (foldl (flip cons) nil l)))

;; scanl :: (a -> b -> a) -> a -> [b] -> [a]
(define scanl
  (lambda (f q l)
    (cons q (if (null? l)
                nil
                (scanl f (f q (car l)) (cdr l))))))

;; scanl1 :: (a -> a -> a) -> [a] -> [a]
(define scanl1
  (lambda (f l)
    (if (null? l)
        nil
        (scanl f (car l) (cdr l)))))

;; scanr :: (a -> b -> b) -> b -> [a] -> [b]
(define scanr
  (lambda (f q0 l)
    (if (null? l)
        (list1 q0)
        (let ((qs (scanr f q0 (cdr l))))
          (cons (f (car l) (car qs)) qs)))))

;; scanr1 :: (a -> a -> a) -> [a] -> [a]
(define scanr1
  (lambda (f l)
    (if (null? l)
        nil
        (if (null? (cdr l))
            l
            (let ((qs (scanr1 f (cdr l))))
              (cons (f (car l) (car qs)) qs))))))

;; sort :: (Ord a) => [a] -> [a]
(define sort (lambda (l) (sort-by compare l)))

;; sortBy :: (a -> a -> Ordering) -> [a] -> [a]
(define sort-by (lambda (f l) (mergesort f l)))

;; mergesort :: (a -> a -> Ordering) -> [a] -> [a]
(define mergesort (lambda (f l) (mergesort* f (map list1 l))))

;; mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
(define mergesort*
  (lambda (f l)
    (cond ((null? l) nil)
          ((null? (cdr l)) (car l))
          (else (mergesort* f (merge-pairs f l))))))

;; merge-pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
(define merge-pairs
  (lambda (f l)
    (cond ((null? l) nil)
          ((null? (cdr l)) l)
          (else (cons (merge f (car l) (car (cdr l)))
                      (merge-pairs f (cdr (cdr l))))))))

;; merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
(define merge
  (lambda (f l r)
    (cond ((null? l) r)
          ((null? r) l)
          (else (if (equal? (f (car l) (car r)) 'gt)
                    (cons (car r) (merge f l (cdr r)))
                    (cons (car l) (merge f (cdr l) r)))))))

;; span :: (a -> Bool) -> [a] -> ([a],[a])
(define span
  (lambda (p l)
    (if (null? l)
        (cons nil nil)
        (if (p (car l))
            (let ((r (span p (cdr l))))
              (cons (cons (car l) (car r)) (cdr r)))
            (cons nil l)))))

;; splitAt :: Int -> [a] -> ([a],[a])
(define split-at (lambda (n l) (cons (take n l) (drop n l))))

;; sum :: (Num a) => [a] -> a
(define sum (lambda (l) (foldl + 0 l)))

;; tail :: [a] -> [a]
(define tail cdr)

;; take :: Int -> [a] -> [a]
(define take
  (lambda (n l)
    (cond ((<= n 0) nil)
          ((null? l) nil)
          (else (cons (car l) (take (- n 1) (cdr l)))))))

;; takeWhile :: (a -> Bool) -> [a] -> [a]
(define take-while
  (lambda (p l)
    (if (null? l)
        nil
        (if (p (car l))
            (cons (car l) (take-while p (cdr l)))
            nil))))

;; transpose :: [[a]] -> [[a]]
(define transpose
  (lambda (l)
    (let ((protect
           (lambda (f)
             (lambda (x)
               (if (null? x)
                   nil
                   (f x))))))
      (cond ((null? l) nil)
            ((null? (car l)) (transpose (cdr l)))
            (else (let* ((e (car l))
                         (x (car e))
                         (xs (cdr e))
                         (xss (cdr l)))
                    (cons (cons x
                                (filter (compose not null?)
                                        (map (protect car) xss)))
                          (transpose (cons xs
                                           (map (protect cdr) xss))))))))))

;; unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
;;
;; (unfoldr (lambda (b) (if (= b 0) #f (cons b (- b 1)))) 10) ; (10 9 8 7 6 5 4 3 2 1)
(define unfoldr
  (lambda (f x)
    (let ((r (f x)))
      (if r
          (cons (car r) (unfoldr f (cdr r)))
          nil))))


;; union :: (Eq a) => [a] -> [a] -> [a]
(define union (lambda (a b) (union-by equal? a b)))

;; unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
(define union-by
  (lambda (f xs ys)
    (let ((g (lambda (x y) (delete-by f y x))))
      (append xs (foldl g (nub-by f ys) xs)))))

;; zip :: [a] -> [b] -> [(a, b)]
(define zip
  (lambda (a b)
    (zip-with cons a b)))

;; zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
(define zip-with
  (lambda (f a b)
    (cond ((null? a) nil)
          ((null? b) nil)
          (else (cons (f (car a) (car b))
                      (zip-with f (cdr a) (cdr b)))))))

;; zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
(define zip-with3
  (lambda (f a b c)
    (cond ((null? a) nil)
          ((null? b) nil)
          ((null? c) nil)
          (else (cons (f (car a) (car b) (car c))
                      (zip-with3 f (cdr a) (cdr b) (cdr c)))))))
