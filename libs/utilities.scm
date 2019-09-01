;; -*- geiser-scheme-implementation: chez-*-
;;----------------------------------------------------------
;; Fundamental utilities
;; ---------------------------------------------------------
(library (utilities)
  (export trunc-int round-down-f pseudo-rand inc dec
          nearly-divisible divisible on-each
          above below clamp
          between between-inclusive between-each
          equal nearly-equal
          range-sine
          identity
          pair first rest cons-r
          merge-columns
          columns-to-rows
          concatenate
          merge-sorted
          pairwise
          unzip-pairs
          extend-repeating-last
          take
          repeat
          sorted?
          for-any
          for-none
          combine-preds
          compose
          list-index
          list-last
          remove-list
          unsafe-list?
          push-front
          alist-get
          alist-set
          alist-get-multi
          alist-let
          make-alist
          derecord
          lif asif
          nor
          declare-keywords
          check-type
          println
          make-safe-val
          safe-val?
          safe-val-apply
          string-contains
          string-contains-ci
          gen-id)

  (import (chezscheme)
          (only (srfi s1 lists)
                take
                first
                list-index)
          (srfi s27 random-bits)
          (only (srfi s13 strings) 
                string-contains
                string-contains-ci)
          (thunder-utils))

  ;; Truncate and integerize
  (define (trunc-int x)
    (exact (truncate x)))

  ;; Find the nearest whole multiple of divisor that's <= x.
  (define (round-down-f x divisor)
    (* divisor (trunc-int (/ x divisor))))

  ;; A pseudo-random number generator that takes a seed.
  (define pseudo-rand-src (make-random-source))

  (define (pseudo-rand min max seed)
    (let* ([i (trunc-int seed)]
           [j (trunc-int (* 100 (- seed i)))]
           [len (- max min)]
           [sign (if (negative? len) -1 1)]
           [len (abs len)])
      (random-source-pseudo-randomize! pseudo-rand-src i j)
      (* sign
         (+ min (if (and (exact? min) (exact? max))
                    ((random-source-make-integers pseudo-rand-src) len)
                    (* len ((random-source-make-reals pseudo-rand-src))))))))

  ;; Some 'english sounding' math operators.
  (define (inc val) (+ val 1))
  (define (dec val) (- val 1))

  (define (nearly-divisible val div error)
    (let* ([remain (/ val div)]
           [truncated (truncate remain)])
      (< (- remain truncated) error)))

  (define (divisible val div)
    (nearly-divisible val div 0.0001))

  (define (on-each val on each)
    (let ([m (mod val each)])
      (= m on)))

  (define (above a b)
    (>= a b))

  (define (below a b)
    (<= a b))

  (define (nearly-equal a b error)
    (< (abs (- a b)) error))

  (define (equal a b)
    (= a b))

  (define (not-equal a b)
    (not (equal a b)))

  (define (between x lower upper)
    (and (>= x lower) (< x upper)))

  (define (between-inclusive x lower upper)
    (and (>= x lower) (<= x upper)))

  (define (between-each x lower upper each)
    (let ([m (mod x each)])
      (between m lower upper)))

  (define (clamp value lo hi)
    (cond
      ((< value lo) lo)
      ((> value hi) hi)
      (else value)))

  (define pi 3.141592653589793)

  (define (range-sine freq lo hi val)
    (+ lo (* (- hi lo) 0.5 (+ 1 (sin (/ (* 2 pi val) freq))))))

  ;; More readable for users to write pair/first/rest
  (define pair cons)
  (define rest cdr)
  (define identity (lambda (x) x))
  (define (cons-r a b) (cons b a))

  ;;-------------------------------------------------------------------
  ;; Takes the head off each inner list until one of
  ;; them runs out. The heads of each column are offered
  ;; to a joiner func, along with an accumulated result.
  (define (column-process col-list joiner)
    (let impl ([l col-list] [result '()])
      (if (member '() l) result
          (impl (map cdr l) (joiner result (map car l))))))

  ;; ((1 2 3) (x y) (a b c)) -> (1 x a 2 y b)
  (define (merge-columns col-list)
    (column-process col-list (lambda (a b) (append a b))))

  ;; ((1 2 3) (x y) (a b c)) -> ((1 x a) (2 y b))
  (define (columns-to-rows col-list)
    (reverse (column-process col-list (lambda (a b) (cons b a)))))

  ;;  ((1 2 3) (x y) (a b c)) -> (1 2 3 x y a b c)
  (define (concatenate col-list)
    (fold-right push-front '() col-list))

  (define (repeat n val)
    (let loop ([x '()] [n n])
      (if (<= n 0) x (loop (cons val x) (- n 1)))))

  ;; Check if a list is sorted or not.
  (define (sorted? less? l)
    (let loop ([l (cdr l)] [prev (car l)])
      (cond
        ((null? l) #t)
        ((or (less? prev (car l))
             (not (less? (car l) prev)))
         (loop (cdr l) (car l)))
        (else #f))))

  ;; Stable merges to lists according to less?. Lifted from
  ;; SRFI95 ref implementation, with tweaks.
  (define (merge-sorted a b less? . opt-key)
    (define key (if (null? opt-key) values (car opt-key)))
    (cond ((null? a) b)
          ((null? b) a)
          (else
            (let loop ((x (car a)) (kx (key (car a))) (a (cdr a))
                                   (y (car b)) (ky (key (car b))) (b (cdr b)))
              ;; The loop handles the merging of non-empty lists.  It has
              ;; been written this way to save testing and car/cdring.
              (if (less? ky kx)
                  (if (null? b)
                      (cons y (cons x a))
                      (cons y (loop x kx a (car b) (key (car b)) (cdr b))))
                  ;; x <= y
                  (if (null? a)
                      (cons x (cons y b))
                      (cons x (loop (car a) (key (car a)) (cdr a) y ky b))))))))

  ;; Takes successive pairs in a flat list and builds a new list with them
  ;; as consed pairs.
  ;; If the input list isn't even numbered, returns #f.
  (define (pairwise lst)
    (nor (even? (length lst))
         (let loop ([lst lst] [pairs '()])
           (if (null? lst)
               (reverse pairs)
               (loop (cddr lst)
                     (cons (cons (car lst) (cadr lst)) pairs))))))

  ;; Unzips an even-numbered list into two lists. 
  ;; If the input list isn't even numbered, returns two #f values.
  ;; (1 2 3 4 5 6 7 8) ->
  ;; (values (1 3 5 7)
  ;;         (2 4 6 8))
  (define (unzip-pairs pairs)
    (if (not (zero? (mod (length pairs) 2)))
        (values #f #f)
        (let loop ([pairs pairs] [keys '()] [vals '()])
          (if (null? pairs)
              (values (reverse keys) (reverse vals))
              (loop (cddr pairs) 
                    (cons (car pairs) keys)
                    (cons (cadr pairs) vals))))))

  ;; Makes the input list have a length of desired-len, either
  ;; by dropping elements at the end or repeating the last list element.
  (define (extend-repeating-last lst desired-len)
    (when (null? lst)
      (error 'extend-by-repeating-last "can't accept null list" lst))
    (let ([to-repeat (list-last lst)]
          [len (length lst)])
      (let loop ([num (- desired-len len)] [end '()])
        (cond
          ((zero? num) (append lst end))
          ((negative? num) (take (+ len num) lst))
          (else (loop (dec num) (cons to-repeat end)))))))

  ;;--------------------------------------------------------------------
  ;; R6RS provides for-all, which checks all items in a
  ;; list return true for pred. Here's the 'none' and
  ;; 'any' versions.
  (define (for-any pred lst)
    (exists pred lst))

  (define (for-none pred lst)
    (not (for-any pred lst)))

  ;; Get a lambda that combines all of the predicates in the
  ;; list. e.g. Supplying for-all/any will check
  ;; all/any of the predicates return true for x.
  (define (combine-preds preds for-all/any/none)
    (lambda (x)
      (for-all/any/none (lambda (p) (p x)) preds)))

  ;; Compose multiple functions that take and return the same arg.
  (define (compose . fns)
    (fold-left (lambda (a b) (lambda (x) (b (a x)))) identity fns))

  ;;----------------------------------------------------------------------
  ;; Get the last element of a list. Linear time.
  (define (list-last l)
    (if (null? (cdr l))
        (car l)
        (list-last (cdr l))))

  ;; Remove matching items in b from a
  (define (remove-list a b)
    (filter (lambda (x) (not (member x b))) a))

  ;; #t for any kind of list: null, proper, improper, or cyclic.
  ;; Faster than 'list?' but improper lists fail with e.g. append.
  (define (unsafe-list? x)
    (or (null? x)
        (and (pair? x)
             (or (null? (cdr x))
                 (pair? (cdr x))))))

  ;; Adds the element to the list. If the element is a list, it is appended.
  (define (push-front val list)
    ((if (unsafe-list? val) append cons) val list))

  ;; Get an alist value, or default if not
  (define (alist-get alist key default)
    (let ([result (assq key alist)])
      (if result (cdr result) default)))

  ;; Set an alist value
  (define (alist-set alist key value)
    (if (and (not (null? alist))
             (eq? key (caar alist)) (eq? value (cadr alist)))
        alist ;; optimise: don't set same value twice in a row
        (cons (cons key value) alist)))

  ;; Get multiple alist values in one go.
  ;; Can be more efficient than searching seperately for each value.
  ;; key-default-pairs should be a list of: (key default-val).
  ;; Returns a new alist containing only the key/value pairs requested.
  (define (alist-get-multi alist key-default-pairs)
    (let* ([targets key-default-pairs]
           [found '()]
           [setter (lambda (entry)
                     (let ([t (assq (car entry) targets)])
                       (when t
                         (set! found (cons entry found))
                         (set! targets (remq t targets)))))])
      (for-each setter alist)
      (append found targets))) ;; Add defaults for those not found.

  ;; A helpful macro to bind to multiple values in an alist.
  (define-syntax alist-let
    (syntax-rules ()
      ((_ alist ([name key default] ...)
          body ...)
       (let* ([found (alist-get-multi alist (list (cons key default) ...))]
              [name (cdr (assq key found))] ...)
         body ...))))

  ;; TODO: perhaps delete, hardly improves pairwise
  (define (make-alist . kv-pairs)
    (asif (alist (pairwise kv-pairs))
           alist
          (error 'make-alist "requires an even list of keys and values.")))

  ;; Useful for destructuring records
  (define-syntax derecord
    (syntax-rules ()
      ((_ rec ([name record-accessor] ...) body-forms ...)
       (let ([name (record-accessor rec)] ...)
         body-forms ...))))

  (define-syntax declare-keywords
    (syntax-rules ()
      ((_ name1 name2 ...) 
       (begin
         (define name1 'name1)
         (define name2 'name2) ...))))

  ;; let+if in a more compact way
  (define-syntax lif
    (syntax-rules ()
      ((_ [name init] test t-branch f-branch)
       (let ([name init])
         (if test t-branch f-branch)))))

  ;; same, but no explicit test - tests the value directly
  (define-syntax asif
    (syntax-rules ()
      ((_ [name init] t-branch f-branch)
       (lif [name init] name t-branch f-branch))))

  ;; return false if the test fails, execute body otherwise
  (define-syntax nor
    (syntax-rules ()
      ((_ test body)
       (if (not test) #f body))))

  ;;------------------------------------------------------------------------
  ;; Throw an error if the wrong type is used
  (define-syntax check-type
    (syntax-rules ()
      ((_ pred val context-id)
       (unless (pred val) 
         (error context-id (format "~A should satisfy ~A" 'val 'pred) val)))))

  (define (println . objs)
    (for-each (lambda (x) (newline) (display x)) objs))

  ;; Helper for synthesising new identifiers in unhygenic macros.
  (define (gen-id template-id . args)
    (define (arg->string x)
      (if (string? x) x (symbol->string (syntax->datum x))))
    (let ([str (apply string-append (map arg->string args))])
      (datum->syntax template-id (string->symbol str))))
  
  ;;------------------------------------------------------------------------
  ;; A value bound with a mutex to make it threadsafe. You should always
  ;; use and access the value through safe-val-apply.
  (define-record-type safe-val
    (fields (immutable mutex)
            (mutable object))
    (protocol
     (lambda (new)
       (lambda (object)
         (new (make-mutex) object)))))

  (define (safe-val-apply obj-method safe-obj . args)
    (with-mutex (safe-val-mutex safe-obj)
      (apply obj-method (safe-val-object safe-obj) args)))

  ) ; end module 'utilities'
