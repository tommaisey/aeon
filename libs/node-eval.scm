(library (node-eval)
  (export
    render-arc
    eval-seq
    eval-seq-empty
    seq-meta-ranged
    seq-subdivider?
    maybe-seq-meta
    seq-meta-rng-max
    seq-meta-rng-min
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
  ;; Get a value from a seq. The source seq might be a plain value, a
  ;; naked contextual function or a 'decorated' seq-meta object.
  (define (eval-seq v context)
    (cond
      [(procedure? v) (eval-seq (v context) context)]
      [(seq-meta? v) (eval-seq (seq-meta-fn v) context)]
      [else v]))

  ;; If we eval a seq in order to add a new event, the context will look
  ;; wrong - the event doesn't yet exist, but some functions relt on that.
  ;; So we add an empty event to the context before evaluating.
  ;; TODO: is this needed? rand seeding doesn't use the event any more.
  ;;       are there other functions that need this?
  (define (eval-seq-empty v time-to-add context)
    (if (or (procedure? v) (seq-meta? v))
        (eval-seq v (context-insert context (make-event time-to-add)))
        v))

  ;; Pop the top lambda off the context's chain, and call it
  ;; with the context itself. This will be done recursively
  ;; up the chain.
  (define (context-resolve c)
    (lif (ch (context-chain c)) (null? ch) c
         (eval-seq (car ch) (context-pop-chain c))))

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
  ;; An object containing a seq function as well as some metadata
  ;; that help to evaluate the tree correctly in some situations.
  ;; A record-writer is defined at the bottom of the file.
  (define-record-type seq-meta
    (fields (immutable rng-min) ;; #f if impossible to detect
            (immutable rng-max) ;; #f if impossible to detect
            (immutable fn)
            (immutable subdivider?)))

  ;; Try to create a seq object, filling in the min and max possible values
  ;; from an input list. Input is filtered for symbols, so ~ rests are ok.
  ;; If we can't work out a min & max value from lst, just returns the raw fn.
  (define seq-meta-ranged
    (case-lambda
      ((lst fn) (seq-meta-ranged #f lst fn))

      ((subdivider? lst fn)
       (let* ([lst (filter (lambda (x) (or (number? x) (seq-meta? x))) lst)]
              [range-min (seq-foldl seq-meta-rng-min min +inf.0 lst)]
              [range-max (seq-foldl seq-meta-rng-max max -inf.0 lst)])
         (cond
           [(and range-min range-max)
            (make-seq-meta range-min range-max fn subdivider?)]
           [subdivider?
            (make-seq-meta #f #f fn subdivider?)]
           [else fn])))))

  (define (seq-subdivider? seq)
    (and (seq-meta? seq)
         (seq-meta-subdivider? seq)))

  ;; Tries to get field from a seq - if it's an undecorated procedure
  ;; we return false. Otherwise we return a primitive value.
  (define (maybe-seq-meta v seq-field)
    (cond
      ((or (eq? v #f)        ;; propagate failure
           (procedure? v)    ;; undecorated fn, can't get metadata
           (unsafe-list? v)) ;; a list, can't get metadata
       #f)
      ((seq-meta? v) (seq-field v))
      (else v)))

  ;; Applies a reduce on a mixed list of primitive values, contextual
  ;; functions and seq objects. Helpful in constructing seq objects.
  (define (seq-foldl seq-field fn start-val values)
    (define (combine result v)
      (lif [meta (maybe-seq-meta v seq-field)]
           (and result meta)
           (fn result meta) #f))
    (fold-left combine start-val values))

  ;; This has to be after all definitions.
  (record-writer
   (type-descriptor seq-meta)
   (lambda (rec p wr)
     (display "#[seq" p)
     (when (seq-meta-subdivider? rec)
       (display " subdiv" p))
     (when (seq-meta-rng-min rec)
       (display " hi:" p)
       (display (number->string (seq-meta-rng-min rec)) p)
       (display " lo:" p)
       (display (number->string (seq-meta-rng-max rec)) p)
       (display "" p))
     (display "]" p)))
  )
