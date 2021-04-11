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
(library (nodes-continuous)
  (export
   ? rnd wpick pick
   each every sine)

  (import
    (chezscheme)
    (utilities)
    (rename (matchable) (? ??))
    (context) (event)
    (node-eval)
    (for (pdef) expand))

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
              [len (length lst)]
              [rgen (rnd 0 len key/keys)])
         (leaf-meta-ranged
          lst
          (lambda (context)
            (let ([choice (eval-leaf rgen context)])
              (eval-leaf (list-ref lst choice) context))))))))

  ;; Choose from a list randomly, with weightings
  (define-syntax wpick
    (syntax-rules ()
      ((_ qlist weights) (wpick qlist weights '()))

      ((_ qlist weights key/keys)

       (let ([lst (pdef qlist)] [wts (pdef weights)])
         (define (to-cumulative lst)
           (let loop ([count 0] [o '()] [lst lst])
             (match lst
               [() (reverse o)]
               [(a . b) (loop (+ count a) (cons (+ count a) o) b)])))

         (define (index-picker cumulative-weights)
           (let ([len (length cumulative-weights)])
             (lambda (v)
               (or (list-index (lambda (x) (>= x v)) cumulative-weights)
                   (- len 1)))))

         (when (or (not (unsafe-list? lst)) (not (unsafe-list? wts)) 
                   (null? lst) (null? wts))
           (error 'wpick "requires 2 lists" lst wts))

         (let* ([len (length lst)]
                [wts (extend-repeating-last wts len)]
                [cumulative-weights (to-cumulative wts)]
                [top (list-last cumulative-weights)]
                [rgen (rnd 0 top key/keys)]
                [picker (index-picker cumulative-weights)])
           (leaf-meta-ranged
            lst
            (lambda (context)
              (let* ([v (eval-leaf rgen context)])
                (eval-leaf (list-ref lst (picker v)) context)))))))))

  ;; This short operator picks either rnd, pick or wpick depending
  ;; on the arguments. Two lists gets wpick, one list gets pick, and
  ;; two non-lists gets rnd. Any other combination of args is illegal.
  (define-syntax ?
    (syntax-rules ()
      ((_ a b key/keys)
       (let ([ad (pdef a)] [bd (pdef b)])
         (unless (or (symbol? key/keys) (unsafe-list? key/keys))
           (error '? "3rd arg must be a key or key list" ad bd key/keys))
         (if (unsafe-list? ad)
             (if (unsafe-list? bd)
                 (wpick ad bd key/keys)
                 (error '? "2nd arg must be a list if 1st is" ad bd))
             (rnd ad bd key/keys))))

      ((_ [a b ...] key/keys)
       (lif [ks (pdef key/keys)]
            (for-all symbol? ks)
            (pick [a b ...] key/keys)
            (? [a b ...] key/keys '())))

      ((_ [a b ...]) (pick [a b ...]))

      ((_ a b) (? a b '()))

      ((_ args ...)
       (error '? "invalid number of args" 'args ...))))

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
              (eval-leaf (list-ref lst (modulo n len)) context))))))
      
      ((_ args ...)
       (error 'each "expects (measures values)" 'args ...))))

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
         (when (zero? n)
           (error 'every "n cannot be 0" n))
         (leaf-meta-ranged
          lst
          (lambda (context)
            (let* ([t (context-now context)]
                   [i (if (zero? t) 0 (trunc-int (/ t measures)))]
                   [n-cycles (trunc-int (/ i n))]
                   [choice (lambda () (+ 1 (mod n-cycles (- len 1))))])
              (lif [n-wrapped (mod i n)]
                   (eq? n-wrapped (- n 1))
                   (eval-leaf (list-ref lst (choice)) context)
                   (eval-leaf (car lst) context)))))))
      
      ((_ args ...)
       (error 'every "expects (N measures values)" 'args ...))))

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
      (match key/keys
        [() (fn init time)]
        [(?? symbol?) (fn init (event-get event key/keys 1))]
        [(?? unsafe-list?)
         (fold-left fn init (event-clean (filter matches-key? event)))])))

  ;; Helper for `this`, `next` and `nearest`.
  (define (get c key default)
    (event-get (context-event c) key default))

  )
