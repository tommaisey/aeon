;;----------------------------------------------------------------------
;; leaves
;;
;; Functions which might return different value based on the context they
;; are passed, i.e. 'contextual' values. This lets us maintain the
;; referential transparency that is key for the system to work.
;;
;; They form the 'leaves' of a tree of functions defining musical patterns.
;;
;; Many of these functions base their value on the time/beat of the current
;; event in the context, unless passed one or more extra keys to look at.
;;
;; Others of these functions don't really need a context - but may want to
;; treat _their_ arguments as callable leaves (which *may* need a context).
;;
;; Many of these actually return a special 'leaf' object, a function wrapped
;; with some metadata that may be needed to evaluate the graph accurately.
;;----------------------------------------------------------------------
(library (value-nodes)
  (export
    this next nearest
    c+ c- c* c/
    rnd pick each every
    snap sine)

  (import
    (chezscheme) (utilities) (context)
    (node-eval) (event)
    (for (pdef) expand))
  
  ;;-------------------------------------------------------------------
  (define (get c key default)
    (event-get (context-event c) key default))

  ;; Get values from the current or neighbouring events in the context.
  (define (this key default)
    (lambda (context)
      (get context key default)))

  (define (next idx key default)
    (lambda (context)
      (get (context-move context idx) key default)))

  (define (nearest time key default)
    (lambda (context)
      (get (context-to-closest-event context time) key default)))

  ;;-------------------------------------------------------------------
  ;; Maths
  (define (leaf-apply fn leaves)
    (lambda (context)
      (apply fn (map (lambda (v) (eval-leaf v context)) leaves))))

  (define (c+ . leaves)
    (leaf-apply + leaves))

  (define (c- . leaves)
    (leaf-apply - leaves))

  (define (c* . leaves)
    (leaf-apply * leaves))

  (define (c/ . leaves)
    (leaf-apply / leaves))

  ;; Snap the input value to the next number divisible by divisor.
  (define (snap divisor val)
    (lambda (context)
      (let* ([val (eval-leaf val context)]
             [divisor (eval-leaf divisor context)]
             [overlap (mod val divisor)]
             [prev (- val overlap)])
        (if (>= overlap (* 0.5 divisor))
            (+ prev divisor) prev))))

  ;;-------------------------------------------------------------------
  ;; Pseudo-random values and choices.
  (define rnd
    (case-lambda
      [() (rnd '())]
      [(key) (rnd 0.0 1.0 key)]
      [(min max) (rnd min max '())]
      [(min max key/keys)
       (leaf-from-list (list min max)
        (lambda (context)
          (let ([seed (fold-by-keys * 10000 key/keys context)])
            (pseudo-rand (eval-leaf min context) (eval-leaf max context) seed))))]))

  ;; Choose from a list randomly
  (define-syntax pick
    (syntax-rules ()
      ((_ qlist) (pick qlist '()))

      ((_ qlist key/keys)
       (let* ([lst (make-pdef-data qlist)]
              [len (length lst)])
         (leaf-from-list lst
          (lambda (context)
            (let ([choice (eval-leaf (rnd 0 len key/keys) context)])
              (eval-leaf (list-nth lst choice) context))))))))

  (tag-pdef-callable pick) ;; Tag so pdef recognises as a macro
  
  ;;--------------------------------------------------------------------
  ;; Rhythmic & sequencing operations.

  ;; Choose from a list according to the current measure.
  (define-syntax each
    (syntax-rules ()
      ((_ measures qlist)
       (let* ([lst (make-pdef-data qlist)]
              [len (length lst)])
         (when (< len 1)
           (error 'each "requires at least 1 value" len))
         (leaf-from-list lst
          (lambda (context)
            (let* ([t (context-now context)]
                   [n (trunc-int (/ t measures))])
              (eval-leaf (list-nth lst (modulo n len)) context))))))))

  ;; Normally chooses the first value, but every n measures chooses
  ;; the second value instead. If there are more than 2 values, the
  ;; 2nd through nth values are cycled.
  (define-syntax every
    (syntax-rules ()
      ((_ n measures qlist)
       (let* ([lst (make-pdef-data qlist)]
              [len (length lst)])
         (when (< len 2)
           (error 'every "requires at least 2 values" len))
         (leaf-from-list lst
          (lambda (context)
            (let* ([t (context-now context)]
                   [i (if (zero? t) 0 (trunc-int (/ t measures)))]
                   [n-wrapped (mod i n)]
                   [n-cycles (trunc-int (/ i n))])
              (if (eq? n-wrapped (- n 1))
                  (eval-leaf (list-ref lst (+ 1 (mod n-cycles (- len 1)))) context)
                  (eval-leaf (car lst) context)))))))))

  ;; Tag so pdef recognises as a macro
  (tag-pdef-callable each)
  (tag-pdef-callable every)

  (define (sine freq lo hi)
    (leaf-from-list (list lo hi)
     (lambda (context)
       (let ([f (eval-leaf freq context)]
             [l (eval-leaf lo context)]
             [h (eval-leaf hi context)])
         (range-sine f l h (context-now context))))))

  ;;--------------------------------------------------------------------
  ;; Some leaves allow the user to specify which properties of the
  ;; context's current event are considered when contextualising. This
  ;; makes the implementation of that simpler.
  (define (fold-by-keys fn init key/keys context)
    (define (matches-key? pair)
      (find (lambda (k) (eq? k (car pair))) key/keys))
    (let ([time (context-now context)]
          [event (context-event context)])
      (cond
        ((null? key/keys)
         (fn init time))
        ((symbol? key/keys)
         (fn init (event-get event key/keys 1)))
        ((unsafe-list? key/keys)
         (fold-left fn init (event-clean (filter matches-key? event)))))))


  )