(library (utilities)

  (export identity compose
          pseudo-rand
          trunc-int round-down round-up round-nearest 
          inc dec clamp 
          between? between-inclusive?
          pi range-sine
          pair first rest cons-r
          sorted? merge-sorted
          pairwise repeat flatten take
          extend-repeating-last
          for-any for-none
          list-index list-last unsafe-list? push-front
          delete-duplicates lset-difference
          find-first-slot
          make-alist
          alist-get alist-set
          alist-set-if-not
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
          string-suffix?
          string-last
          str+
          gen-id
          with-identifiers
          lambda*
          define*
          /opt)

  (import (chezscheme)
          (matchable)
	  (srfi s27 random-bits)
          (only (srfi s1 lists)
                first
                list-index
                delete-duplicates
                lset-difference)
          (only (srfi s13 strings) 
                string-contains
                string-contains-ci
		string-suffix?)
          (only (thunder-utils)
		lambda/optional
		define/optional
		/optional))

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
  (define round-int (compose exact round))

  ;; Find the nearest whole multiple of divisor that's <= x.
  (define (round-down x divisor)
    (* divisor (floor-int (/ x divisor))))

  (define (round-up x divisor)
    (* divisor (ceil-int (/ x divisor))))

  (define (round-nearest x divisor)
    (let ([r (if (exact? divisor) round-int round)])
      (* (r (/ x divisor)) divisor)))

  (define (between? x lower upper)
    (and (>= x lower) (< x upper)))

  (define (between-inclusive? x lower upper)
    (and (>= x lower) (<= x upper)))

  (define (clamp value lo hi)
    (cond
      [(< value lo) lo]
      [(> value hi) hi]
      [else value]))

  (define pi 3.141592653589793)

  (define (range-sine freq lo hi val)
    (+ lo (* (- hi lo) 0.5 (+ 1 (sin (/ (* 2 pi val) freq))))))

  ;; More readable for users to write pair/first/rest
  (alias pair cons)
  (alias rest cdr)
  (define (cons-r a b) (cons b a))

  ;;-------------------------------------------------------------------
  ;; (1 (2 3) 4 (5 6 (7 8))) -> (1 2 3 4 5 6 7 8)
  ;; n.b. converts improper lists to proper lists.
  (define (flatten lst)
    (let loop ([lst lst] [acc '()])
      (match lst
        [() acc]
        [(a . b) (loop a (loop b acc))]
        [x (cons x acc)])))

  (define (repeat n val)
    (let loop ([x '()] [n n])
      (if (<= n 0) x (loop (cons val x) (dec n)))))

  ;; Makes the input list have a length of desired-len, either
  ;; by dropping elements at the end or repeating the last list element.
  (define (extend-repeating-last lst desired-len)
    (when (null? lst)
      (error 'extend-by-repeating-last "can't accept null list" lst))
    (let ([to-repeat (list-last lst)]
          [len (length lst)])
      (let loop ([num (- desired-len len)] [end '()])
        (cond
          [(zero? num) (append lst end)]
          [(negative? num) (take lst (max 0 (+ len num)))]
          [else (loop (dec num) (cons to-repeat end))]))))

  ;; Check if a list is sorted or not.
  (define (sorted? less? lst)
    (or (null? lst) (null? (cdr lst))
        (let ([a (car lst)] [b (cadr lst)])
          (and (or (less? a b) (not (less? b a)))
               (sorted? less? (cdr lst))))))

  ;; Stable merges two lists according to less?. Lifted from
  ;; SRFI95 ref implementation, with tweaks.
  (define (merge-sorted less? a b . opt-key)
    (define key (if (null? opt-key) values (car opt-key)))
    (cond [(null? a) b]
          [(null? b) a]
          [else
            (let loop ([x (car a)] [kx (key (car a))] [a (cdr a)]
                       [y (car b)] [ky (key (car b))] [b (cdr b)])
              ;; The loop handles the merging of non-empty lists.  It has
              ;; been written this way to save testing and car/cdring.
              (if (less? ky kx)
                  (if (null? b)
                      (cons y (cons x a))
                      (cons y (loop x kx a (car b) (key (car b)) (cdr b))))
                  ;; x <= y
                  (if (null? a)
                      (cons x (cons y b))
                      (cons x (loop (car a) (key (car a)) (cdr a) y ky b)))))]))

  ;; Takes successive pairs in a flat list and builds a new list with them
  ;; as consed pairs.
  ;; If the input list isn't even numbered, returns #f.
  (define (pairwise lst)
    (let loop ([lst lst] [pairs '()])
      (match lst
        [() (reverse pairs)]
        [(x) #f]
        [(a b . lst) (loop lst (cons (cons a b) pairs))])))

  ;;-------------------------------------------------------------------
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
  (alias for-any exists)

  (define (for-none pred lst)
    (not (exists pred lst)))

  ;;----------------------------------------------------------------------
  ;; Lists & vectors

  ;; #t for any kind of list: null, proper, improper, or cyclic.
  ;; Faster than 'list?' but improper lists (which return true)
  ;; are dangerous and shouldn't be used with e.g. append.
  (define (unsafe-list? x)
    (match x
      [(a . b) (or (null? b) (pair? b))]
      [a (null? a)]))

  ;; Linear time.
  (define (list-last lst)
    (match lst
      [(x . ()) x]
      [(a . x) (list-last x)]
      [lst lst])) ;; improper list

  ;; Remove matching items in b from a
  (define (remove-list a b)
    (filter (lambda (x) (not (member x b))) a))

  ;; Adds the element to the list. If the element is a list, it is appended.
  (define (push-front val lst)
    ((if (unsafe-list? val) append cons) val lst))

  ;; Return the first n items of the list
  (define (take lst n)
    (if (or (null? lst) (<= n 0)) '()
        (cons (car lst) (take (cdr lst) (dec n)))))

  ;; Return index of first slot in a vector matching pred. Returns #f otherwise.
  (define* (find-first-slot vec [/opt (pred identity)])
    (let loop ([n 0] [len (vector-length vec)])
      (cond
        [(>= n len) #f]
        [(pred (vector-ref vec n)) n]
        [else (loop (inc n) len)])))

  ;;----------------------------------------------------------------------
  ;; Alists - i.e. association lists

  ;; Get an alist value, or default if not
  (define (alist-get alist key default)
    (lest [result (assq key alist)]
          (cdr result) default))

  ;; Set an alist value
  (define (alist-set alist key value)
    (match alist
      [((k . v) . x)
       (if (and (eq? k key) (eq? v value))
           alist (cons (cons key value) alist))]
      [else (error 'alist-set "doesn't appear to be an alist" alist)]))
  
  ;; Set a value in an alist if it's not already present.
  (define (alist-set-if-not alist key value)
    (if (alist-get alist key #f) alist
        (alist-set alist key value)))
  
  ;; Get multiple alist values in one go.
  ;; Can be more efficient than searching seperately for each value.
  ;; key-default-pairs should be an alist of: (key default-val).
  ;; Returns a new alist containing only the key/value pairs requested.
  (define (alist-get-multi alist key-default-pairs)
    (let loop ([found '()]
               [alist alist]
               [targets key-default-pairs])
      (if (or (null? targets) (null? alist))
          (append found targets) ;; Add defaults for those not found.
          (lest [t (assq (caar alist) targets)]
                (loop (cons (car alist) found) (cdr alist)
                      (remq t targets))
                (loop found (cdr alist) targets)))))

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
    (lest [alist (pairwise kv-pairs)] alist
          (error 'make-alist kv-pairs-err kv-pairs)))

  ;;------------------------------------------------------------------------
  ;; Strings
  (alias str+ string-append)
  
  (define* (string-last str n [/opt (prefix "")])
    (let ([len (string-length str)])
      (if (>= n len) str
          (str+ prefix (substring str (max 0 (- len n)) len)))))

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

  ;; Introduces keywords unhygenically into a macro's scope.
  (define-syntax with-identifiers
    (syntax-rules ()
      ((_ template-id (id ...) body)
       (with-syntax ([id (dat->syn template-id 'id)] ...) body))))

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
         (with-syntax ([make-fn (gen-id #'k "make-" #'record-name)]
                       [(with-fn ...) (gen-ids #'k #'record-name "-with-" #'(field ...))]
                       [(get-fn ...)  (gen-ids #'k #'record-name "-" #'(field ...))])
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
