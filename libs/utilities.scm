;; -*- geiser-scheme-implementation: chez-*-
;;----------------------------------------------------------
;; Fundamental utilities
;; ---------------------------------------------------------
(library (utilities)
  (export identity compose
          pseudo-rand
          trunc-int round-down round-up round-nearest 
          inc dec clamp
          nearly-divisible divisible on-each
          between between-inclusive between-each
          equal nearly-equal
          range-sine
          pair first rest cons-r
          merge-columns
          columns-to-rows
          concatenate
          merge-sorted
          pairwise
          extend-repeating-last
          take
          repeat
          sorted?
          for-any
          for-none
          list-index
          list-last
          unsafe-list?
          push-front
          find-first-slot
          make-alist
          alist-get
          alist-set
          alist-get-multi
          alist-let
          lif lest
          declare-keywords
          check-type
          maybe-convert
          derecord
          define-immutable-record
          println
          make-safe-val
          safe-val?
          safe-val-apply
          string-contains
          string-contains-ci
          string-last
          gen-id
          lambda*
          define*
          /opt)

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

  ;;-------------------------------------------------------------------
  ;; Optional arguments
  (alias lambda* lambda/optional) ;; from thunderutils
  (alias define* define/optional) ;; from thunderutils
  (alias /opt /optional)

  ;;-------------------------------------------------------------------
  ;; Base case for functional composition
  (define identity (lambda (x) x))

  ;; Compose multiple functions that take and return the same arg.
  (define (compose . fns)
    (fold-left (lambda (a b) (lambda (x) (b (a x)))) identity fns))

  ;;-------------------------------------------------------------------
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

  ;;-------------------------------------------------------------------
  ;; Maths
  (define (inc val) (+ val 1))
  (define (dec val) (- val 1))

  ;; Truncate and integerize
  (define trunc-int (compose exact truncate))
  (define floor-int (compose exact floor))
  (define ceil-int  (compose exact ceiling))

  ;; Find the nearest whole multiple of divisor that's <= x.
  (define (round-down x divisor)
    (* divisor (floor-int (/ x divisor))))

  (define (round-up x divisor)
    (* divisor (ceil-int (/ x divisor))))

  (define (round-nearest x divisor)
    (let ([down (round-down x divisor)]
          [up (round-up x divisor)])
      (if (< (- up x) (- x down)) up down)))

  (define (nearly-divisible val div error)
    (let* ([remain (/ val div)]
           [truncated (truncate remain)])
      (< (- remain truncated) error)))

  (define (divisible val div)
    (nearly-divisible val div 0.0001))

  (define (on-each val on each)
    (let ([m (mod val each)])
      (= m on)))

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
  (define (cons-r a b) (cons b a))

  ;;-------------------------------------------------------------------
  ;; Takes the head off each inner list until one of
  ;; them runs out. The heads of each column are offered
  ;; to a joiner func, along with an accumulated result.
  (define (column-process col-list joiner)
    (let loop ([l col-list] [result '()])
      (if (member '() l) result
          (loop (map cdr l) (joiner result (map car l))))))

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
    (and (even? (length lst))
         (let loop ([lst lst] [pairs '()])
           (if (null? lst)
               (reverse pairs)
               (loop (cddr lst)
                     (cons (cons (car lst) (cadr lst)) pairs))))))

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
  ;; Logic
  
  ;; let+if
  (define-syntax lif
    (syntax-rules ()
      ((_ [name init] test t-branch f-branch)
       (let ([name init])
         (if test t-branch f-branch)))))

  ;; same, but no explicit test - tests the value directly
  (define-syntax lest
    (syntax-rules ()
      ((_ [name init] t-branch f-branch)
       (lif [name init] name t-branch f-branch))

      ((_ [name init] t-branch)
       (let ([name init])
         (when name t-branch)))))

  ;; For symmetry with R6RS's for-all.
  (define (for-any pred lst)
    (exists pred lst))

  (define (for-none pred lst)
    (not (for-any pred lst)))

  ;;----------------------------------------------------------------------
  ;; Lists

  ;; #t for any kind of list: null, proper, improper, or cyclic.
  ;; Faster than 'list?' but improper lists (which return true)
  ;; are dangerous and shouldn't be used with e.g. append.
  (define (unsafe-list? x)
    (or (null? x)
        (and (pair? x)
             (or (null? (cdr x))
                 (pair? (cdr x))))))
  
  (define (list-last l) ; Linear time.
    (if (null? (cdr l))
        (car l)
        (list-last (cdr l))))

  ;; Remove matching items in b from a
  (define (remove-list a b)
    (filter (lambda (x) (not (member x b))) a))

  ;; Adds the element to the list. If the element is a list, it is appended.
  (define (push-front val list)
    ((if (unsafe-list? val) append cons) val list))

  ;; Return index of first slot in a vector matching pred. Returns #f otherwise.
  (define* (find-first-slot vec [/opt (pred identity)])
    (let loop ([n 0] [len (vector-length vec)])
      (cond
        ((>= n len) #f)
        ((pred (vector-ref vec n)) n)
        (else (loop (inc n) len)))))

  ;; Get an alist value, or default if not
  (define (alist-get alist key default)
    (lest [result (assq key alist)]
          (cdr result) default))

  ;; Set an alist value
  (define (alist-set alist key value)
    (if (and (not (null? alist))
             (eq? key (caar alist))
             (eq? value (cadr alist)))
        alist ;; optimise: don't set same value twice in a row
        (cons (cons key value) alist)))

  ;;----------------------------------------------------------------------
  ;; Alists - i.e. association lists
  
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
  ;; TODO: could this be faster using a vector internally?
  (define-syntax alist-let
    (syntax-rules ()
      ((_ alist ([name key default] ...)
          body ...)
       (let ([found (alist-get-multi alist (list (cons key default) ...))])
         (let ([name (cdr (assq key found))] ...)
           body ...)))))

  ;; Some error strings used in alist functions below:
  (define kv-pairs-err "requires an even list of keys and values.")
  (define optional-pairs-err
    "optional argument must be key-value pairs, e.g. (:foo 99 :bar 88)")

  ;; Turns a flat list into an alist, or emits an error if they aren't pairs
  (define (make-alist . kv-pairs)
    (lest (alist (pairwise kv-pairs)) alist
          (error 'make-alist kv-pairs-err kv-pairs)))
  
  ;;------------------------------------------------------------------------
  ;; Strings
  (define (string-last str n)
    (let ([len (string-length str)])
      (substring str (max 0 (- len n)) len)))

  (define (println . objs)
    (fresh-line)
    (for-each (lambda (x) (display x) (fresh-line)) objs))

  (define-syntax declare-keywords
    (syntax-rules ()
      ((_ name1 name2 ...) 
       (begin
         (define name1 'name1)
         (define name2 'name2) ...))))

  ;;------------------------------------------------------------------------
  ;; Types
  (define-syntax check-type
    (syntax-rules ()
      ((_ pred val context-id)
       (unless (pred val) 
         (error context-id (format "~A should satisfy ~A" 'val 'pred) val)))))

  ;; If the object matches pred, apply converter to it
  (define (maybe-convert x pred converter)
    (if (pred x) (converter x) x))

  ;;------------------------------------------------------------------------
  ;; These make writing unhygenic macros less verbose
  (alias sym->str symbol->string)
  (alias str->sym string->symbol)
  (alias dat->syn datum->syntax)
  (alias syn->dat syntax->datum)

  ;; Synthesises a symbol from strings and syntax objects
  (meta define (synth-symbol . args)
    (define (arg->string x)
      (if (string? x) x (sym->str (syn->dat x))))
    (str->sym (apply string-append (map arg->string args))))

  ;; Synthesises an identifier for use in an unhygenic macro
  (meta define (gen-id template-id . args)
    (dat->syn template-id (apply synth-symbol args)))

  ;;------------------------------------------------------------------------
  ;; Useful for destructuring records
  (define-syntax derecord
    (syntax-rules ()
      ((_ rec ([name record-accessor] ...) body-forms ...)
       (let ([name (record-accessor rec)] ...)
         body-forms ...))))

  ;; Defines a record with all fields being immutable. Also defines
  ;; functions of the form `record-with-field`, e.g:
  ;;
  ;; (define-immutable-record point (x 0) (y 1))
  ;; (make-point) => (point (x 0) (y 1))
  ;; (make-point 3) => (point (x 3) (y 1))
  ;; (point-with-x (make-point 3 4) 99) => (point (x 99) (y 4))
  (define-syntax define-immutable-record
    (lambda (x)

      (define (gen-ids k rec-name conj-str fields)
        (dat->syn k (map (lambda (f) (synth-symbol rec-name conj-str f)) (syn->dat fields))))

      (syntax-case x ()
        ((k record-name (field default) ...)
         (with-syntax ((make-fn (gen-id #'k "make-" #'record-name))
                       ((with-fn ...) (gen-ids #'k #'record-name "-with-" #'(field ...)))
                       ((get-fn ...)  (gen-ids #'k #'record-name "-" #'(field ...))))
           #'(begin
               (define-record-type record-name
                 (fields (immutable field) ...)
                 (protocol
                  (lambda (new)
                    (lambda* ([/opt [field default] ...])
                      (new field ...)))))

               (define (with-fn record new-value)
                 (let ([field (get-fn record)] ...)
                   (let ([field new-value])
                     (make-fn field ...)))) ...))))))

  ;;------------------------------------------------------------------------
  ;; A value bound with a mutex to make it threadsafe. You should always
  ;; use and access the value through safe-val-apply.
  (define-record-type safe-val
    (fields (immutable mutex)
            (mutable object))
    (protocol (lambda (new) (lambda (object) (new (make-mutex) object)))))

  (define (safe-val-apply obj-method safe-obj . args)
    (with-mutex (safe-val-mutex safe-obj)
      (apply obj-method (safe-val-object safe-obj) args)))

  ) ; end module 'utilities'
