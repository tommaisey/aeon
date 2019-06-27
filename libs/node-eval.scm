(library (node-eval)
  (export
    render
    eval-leaf
    eval-leaf-early
    leaf-meta-ranged
    make-leaf-wanting-transform
    leaf-wanting-transform?
    maybe-leaf-meta
    leaf-foldl
    leaf-meta-range-max
    leaf-meta-range-min
    context-resolve)

  (import (scheme) (utilities) (event) (context))

  ;;--------------------------------------------------------------
  ;; Call on the root of a tree to fill a context with events.
  (define (render p start end)
    (render-ctxt p (make-context (make-arc start end))))

  (define (render-ctxt item context)
    (context-trim (item context)))

  ;;--------------------------------------------------------------
  ;; Get a value from a leaf. The source leaf v might be a plain value, a
  ;; naked contextual function or a special 'decorated' leaf object.
  (define (eval-leaf v context)
    (cond
      ((procedure? v) (eval-leaf (v context) context))
      ((leaf-meta? v) (eval-leaf (leaf-meta-fn v) context))
      (else v)))

  ;; If we eval a leaf in order to add a new event, the context will look
  ;; wrong - the event doesn't yet exist, so e.g. rand seeding would be broken.
  ;; In this case, add an empty event to the context before evaluating.
  (define (eval-leaf-early v time-to-add context)
    (if (or (procedure? v) (leaf-meta? v))
        (eval-leaf v (context-insert context (make-event time-to-add)))
        v))

  ;; Pop the top lambda off the context's chain, and call it
  ;; with the context itself. This will be done recursively
  ;; up the chain.
  (define (context-resolve c)
    (lif (ch (context-chain c)) (null? ch) c
         (eval-leaf (car ch) (context-pop-chain c))))

  ;;-------------------------------------------------------------------
  ;; An object containing a contextual function as well as some metadata
  ;; that are needed to evaluate the graph correctly in some situations.
  (define-record-type leaf-meta
    (fields (immutable range-min) ;; #f if impossible to detect
            (immutable range-max) ;; #f if impossible to detect
            (immutable fn)
            (immutable wants-transform?)))

  ;; Try to create a leaf object, filling in the min and max possible values
  ;; from an input list. Input is filtered for symbols, so ~ rests are ok.
  ;; If we can't work out a min & max value from lst, just returns the raw fn.
  (define (leaf-meta-ranged lst fn)
    (let* ([lst (filter (lambda (x) (not (symbol? x))) lst)]
           [range-min (leaf-foldl leaf-meta-range-min min +inf.0 lst)]
           [range-max (leaf-foldl leaf-meta-range-max max -inf.0 lst)])
      (if (and range-min range-max)
          (make-leaf-meta range-min range-max fn #f) fn)))

  (define (make-leaf-wanting-transform fn)
    (make-leaf-meta #f #f fn #t))

  (define (leaf-wanting-transform? leaf)
    (and (leaf-meta? leaf)
         (leaf-meta-wants-transform? leaf)))

  ;; Tries to get field from a leaf - if it's an undecorated procedure
  ;; we return false. Otherwise we return a primitive value.
  (define (maybe-leaf-meta v leaf-field)
    (cond
      ((eq? v #f) #f)       ;; propagate failure
      ((procedure? v) #f)   ;; undecorated fn, can't get metadata
      ((unsafe-list? v) #f) ;; a list, can't get metadata
      ((leaf-meta? v) (leaf-field v))
      (else v)))

  ;; Applies a reduce on a mixed list of primitive values, contextual
  ;; functions and leaf objects. Helpful in constructing leaf objects.
  (define (leaf-foldl leaf-field fn start-val values)
    (define (combine result v)
      (lif [meta (maybe-leaf-meta v leaf-field)]
            meta (fn result meta) #f))
    (fold-left combine start-val values))
  )
