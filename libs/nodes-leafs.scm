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
(library (nodes-leafs)
  (export
    this next
    c+ c- c* c/
    ? rnd wpick pick
    each every
    snap sine)

  (import
    (chezscheme)
    (utilities)  (context) (event)
    (node-eval)
    (for (pdef) expand))

  ;;-------------------------------------------------------------------
  ;; Get values from the current or neighbouring events in the context.
  (define (this key default)
    (lambda (context)
      (get context key default)))

  (define (next idx key default)
    (lambda (context)
      (get (context-move context idx) key default)))

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
       (leaf-meta-ranged (list min max)
                         (lambda (context)
                           (let ([min (eval-leaf min context)]
                                 [max (eval-leaf max context)]
                                 [seed (fold-by-keys * 10000 key/keys context)])
                             (pseudo-rand min max seed))))]))

  ;; Choose from a list randomly
  (define-syntax pick
    (syntax-rules ()
      ((_ qlist) (pick qlist '()))

      ((_ qlist key/keys)
       (let* ([lst (pdef qlist)]
              [len (length lst)])
         (leaf-meta-ranged
          lst
          (lambda (context)
            (let ([choice (eval-leaf (rnd 0 len key/keys) context)])
              (eval-leaf (list-ref lst choice) context))))))))

  ;; Choose from a list randomly, with weightings
  (define-syntax wpick
    (syntax-rules ()
      ((_ qlist weights) (wpick qlist weights '()))

      ((_ qlist weights key/keys)

       (let ([lst (pdef qlist)])
         (define (to-cumulative lst)
           (let loop ([cnt 0] [o '()] [lst lst])
             (if (null? lst)
                 (reverse o)
                 (let ([nxt-cnt (+ cnt (car lst))])
                   (loop nxt-cnt (cons nxt-cnt o) (cdr lst))))))

         (define (pick-index v cumulative-weights)
           (lif (i (list-index (lambda (x) (>= x v)) cumulative-weights))
                i i (- (length cumulative-weights) 1)))

         (when (or (null? lst) (null? weights))
           (error 'wpick "requires 2 lists" lst weights))

         (let* ([len (length lst)]
                [weights (extend-repeating-last (pdef weights) len)]
                [cumulative-weights (to-cumulative weights)]
                [top (list-last cumulative-weights)])
           (leaf-meta-ranged
            lst
            (lambda (context)
              (let* ([v (eval-leaf (rnd 0 top key/keys) context)]
                     [choice (pick-index v cumulative-weights)])
                (eval-leaf (list-ref lst choice) context)))))))))

  ;; This short operator picks either rnd, pick or wpick depending
  ;; on the arguments. Two lists gets wpick, one list gets pick, and
  ;; two non-lists gets rnd. Any other combination of args is illegal.
  (define-syntax ?
    (syntax-rules ()
      ((_ a b key/keys)
       (let ([ad (pdef a)]
             [bd (pdef b)])
         (when (not (or (symbol? key/keys) (unsafe-list? key/keys)))
           (error '? "3rd arg must be a key or key list;" ad bd key/keys))
         (if (unsafe-list? ad)
             (begin
               (when (not (unsafe-list? bd))
                 (error '? "2nd arg must be a list if 1st is;"  ad bd))
               (wpick ad bd key/keys))
             (rnd ad bd key/keys))))

      ((_ a b) (? a b '()))

      ((_ [a b ...]) (pick [a b ...]))

      ((_ a b ...)
       (error '? "invalid number of args" 'a 'b ...))))

  ;; Tag so pdef recognises as a macro
  (tag-pdef-callable pick)
  (tag-pdef-callable wpick)
  (tag-pdef-callable ?)

  ;;--------------------------------------------------------------------
  ;; Rhythmic & sequencing operations.

  ;; Choose from a list according to the current measure.
  (define-syntax each
    (syntax-rules ()
      ((_ measures qlist)
       (let* ([lst (pdef qlist)]
              [len (length lst)])
         (when (< len 1)
           (error 'each "requires at least 1 value" len))
         (leaf-meta-ranged
          lst
          (lambda (context)
            (let* ([t (context-now context)]
                   [n (trunc-int (/ t measures))])
              (eval-leaf (list-ref lst (modulo n len)) context))))))))

  ;; Normally chooses the first value, but every n measures chooses
  ;; the second value instead. If there are more than 2 values, the
  ;; 2nd through nth values are cycled each n measures.
  (define-syntax every
    (syntax-rules ()
      ((_ n measures qlist)
       (let* ([lst (pdef qlist)]
              [len (length lst)])
         (when (< len 2)
           (error 'every "requires at least 2 values" len))
         (leaf-meta-ranged
          lst
          (lambda (context)
            (let* ([t (context-now context)]
                   [i (if (zero? t) 0 (trunc-int (/ t measures)))]
                   [n-cycles (trunc-int (/ i n))]
                   [choice (lambda () (+ 1 (mod n-cycles (- len 1))))])
              (lif (n-wrapped (mod i n))
                   (eq? n-wrapped (- n 1))
                   (eval-leaf (list-ref lst (choice)) context)
                   (eval-leaf (car lst) context)))))))))

  ;; Tag so pdef recognises as a macro
  (tag-pdef-callable each)
  (tag-pdef-callable every)

  (define (sine freq lo hi)
    (leaf-meta-ranged
     (list lo hi)
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

  ;; Helper for `this`, `next` and `nearest`.
  (define (get c key default)
    (event-get (context-event c) key default))

  )
