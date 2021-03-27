(library (node-eval)
  (export
    render-arc
    eval-leaf
    eval-leaf-empty
    leaf-meta-ranged
    leaf-subdivider?
    maybe-leaf-meta
    leaf-meta-rng-max
    leaf-meta-rng-min
    context-resolve
    make-caching-context)

  (import (scheme) (utilities) (matchable) (arc) (event) (context))

  ;;--------------------------------------------------------------
  ;; Call on the root of a tree to fill a context with events.
  (define render-arc
    (case-lambda
      ((pattern-fn arc) 
       (context-trim (pattern-fn (make-context arc))))

      ((pattern-fn start end) 
       (render-arc pattern-fn (make-arc start end)))))

  ;;--------------------------------------------------------------
  ;; Get a value from a leaf. The source leaf v might be a plain value, a
  ;; naked contextual function or a special 'decorated' leaf object.
  (define (eval-leaf v context)
    (cond
      [(procedure? v) (eval-leaf (v context) context)]
      [(leaf-meta? v) (eval-leaf (leaf-meta-fn v) context)]
      [else v]))

  ;; If we eval a leaf in order to add a new event, the context will look
  ;; wrong - the event doesn't yet exist, so e.g. rand seeding would be broken.
  ;; In this case, add an empty event to the context before evaluating.
  (define (eval-leaf-empty v time-to-add context)
    (if (or (procedure? v) (leaf-meta? v))
        (eval-leaf v (context-insert context (make-event time-to-add)))
        v))

  ;; Pop the top lambda off the context's chain, and call it
  ;; with the context itself. This will be done recursively
  ;; up the chain.
  (define (context-resolve c)
    (lif (ch (context-chain c)) (null? ch) c
         (eval-leaf (car ch) (context-pop-chain c))))

  ;; Makes a context with a special caching function added behind the top fn
  ;; in the chain. Repeated requests for the same arc return a cached result.
  (define (make-caching-context context)
    (define (test-arc arc-fn c1 c2)
      (arc-fn (context-arc c1) (context-arc c2)))
    (define (make-cacher)
      (let ([cached-list '()])
        (lambda (ctxt)
          (let loop ([cl cached-list])
            (match cl
              [() (begin
                    (set! cached-list (cons (context-resolve ctxt) cached-list))
                    (car cached-list))]
              [(c . rst) (cond [(test-arc arc-eq? c ctxt) c]
                               [(test-arc arc-contains? c ctxt)
                                (context-trim (rearc c (context-arc ctxt)))]
                               [else (loop rst)])])))))

    (lif (chain (context-chain context)) (null? chain) context
         (context-push-chain (context-pop-chain context)
                             (list (car chain) (make-cacher)))))

  ;;-------------------------------------------------------------------
  ;; An object containing a contextual function as well as some metadata
  ;; that are needed to evaluate the graph correctly in some situations.
  (define-record-type leaf-meta
    (fields (immutable rng-min) ;; #f if impossible to detect
            (immutable rng-max) ;; #f if impossible to detect
            (immutable fn)
            (immutable subdivider?)))

  ;; Try to create a leaf object, filling in the min and max possible values
  ;; from an input list. Input is filtered for symbols, so ~ rests are ok.
  ;; If we can't work out a min & max value from lst, just returns the raw fn.
  (define leaf-meta-ranged
    (case-lambda
      ((lst fn) (leaf-meta-ranged #f lst fn))

      ((subdivider? lst fn)
       (let* ([lst (filter (lambda (x) (or (number? x) (leaf-meta? x))) lst)]
              [range-min (leaf-foldl leaf-meta-rng-min min +inf.0 lst)]
              [range-max (leaf-foldl leaf-meta-rng-max max -inf.0 lst)])
         (cond
           [(and range-min range-max)
            (make-leaf-meta range-min range-max fn subdivider?)]
           [subdivider?
            (make-leaf-meta #f #f fn subdivider?)]
           [else fn])))))

  (define (leaf-subdivider? leaf)
    (and (leaf-meta? leaf)
         (leaf-meta-subdivider? leaf)))

  ;; Tries to get field from a leaf - if it's an undecorated procedure
  ;; we return false. Otherwise we return a primitive value.
  (define (maybe-leaf-meta v leaf-field)
    (cond
      ((or (eq? v #f)        ;; propagate failure
           (procedure? v)    ;; undecorated fn, can't get metadata
           (unsafe-list? v)) ;; a list, can't get metadata
       #f)
      ((leaf-meta? v) (leaf-field v))
      (else v)))

  ;; Applies a reduce on a mixed list of primitive values, contextual
  ;; functions and leaf objects. Helpful in constructing leaf objects.
  (define (leaf-foldl leaf-field fn start-val values)
    (define (combine result v)
      (lif [meta (maybe-leaf-meta v leaf-field)]
           (and result meta)
           (fn result meta) #f))
    (fold-left combine start-val values))
  )
