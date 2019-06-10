;; -*- geiser-scheme-implementation: chez-*-
;;----------------------------------------------------------
;; Fundamental utilities
;; ---------------------------------------------------------
(library (utilities)
  (export trunc-int round-down-f pseudo-rand inc dec
          nearly-divisible divisible on-each
          above below
          between between-inclusive between-each
          equal nearly-equal
          range-sine
          pair first rest
          merge-columns
          columns-to-rows
          concatenate
          merge-sorted
          repeat
          sorted?
          for-any
          for-none
          combine-preds
          list-nth
          list-last
          remove-list
          unsafe-list?
          push-front
          alist-get-multi
          alist-let
          make-alist
          derecord
          declare-keyword
          check-type
          println
          define/optional
          make-safe-val
          safe-val?
          safe-val-apply
          string-contains string-contains-ci
          gen-id)

  (import (chezscheme)
          (srfi s27 random-bits)
          (only (srfi s13 strings) string-contains string-contains-ci)
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
           [len (- max min)])
      (random-source-pseudo-randomize! pseudo-rand-src i j)
      (+ min (if (and (exact? min) (exact? max))
                 ((random-source-make-integers pseudo-rand-src) len)
                 (* len ((random-source-make-reals pseudo-rand-src)))))))

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

  (define pi 3.141592653589793)

  (define (range-sine freq lo hi val)
    (+ lo (* (- hi lo) 0.5 (+ 1 (sin (/ (* 2 pi val) freq))))))

  ;; More readable for users to write pair/first/rest
  (define pair cons)
  (define first car)
  (define rest cdr)

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

  (define (repeat val n)
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

  ;;----------------------------------------------------------------------
  ;; Get the nth element of a list. Linear time.
  (define (list-nth l n)
    (cond
      ((null? l) '())
      ((eq? n 0) (car l))
      (else (list-nth (cdr l) (- n 1)))))

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

  ;; Get multiple alist values in one go. Returns a new alist
  ;; containing only the key/value pairs requested.
  ;; More efficient than searching seperately for each value.
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

  (define (make-alist . kv-pairs)
    (if (even? (length kv-pairs))
        (do ([pairs kv-pairs (cddr pairs)]
             [alist '() (cons (cons (car pairs) (cadr pairs)) alist)])
            ((null? pairs) (reverse alist)))
        (raise "make-alist requires an even list of keys and values.")))

  ;; Useful for destructuring records
  (define-syntax derecord
    (syntax-rules ()
      ((_ rec ([name record-accessor] ...) body-forms ...)
       (let ([name (record-accessor rec)] ...)
         body-forms ...))))

  (define-syntax declare-keyword
    (syntax-rules ()
      ((_ name) (define name 'name))))

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
    (let* ([str (lambda (x) (if (string? x) x (symbol->string (syntax->datum x))))]
           [sym (string->symbol (apply string-append (map str args)))])
      (datum->syntax template-id sym)))

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
