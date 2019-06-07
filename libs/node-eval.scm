(library (node-eval)
  (export
    get-leaf
    get-leaf-early
    render)

  (import (scheme) (utilities) (event) (context))

  ;;--------------------------------------------------------------
  ;; Call on the root of a tree to fill a context with events.
  (define (render p start end)
    (render-ctxt p (make-context (make-arc start end))))

  (define (render-ctxt item context)
    (context-trim (item context)))

  ;; Evaluate a leaf. It might be a plain value, a naked context
  ;; procedure or an arc-moving-branch.
  (define (get-leaf leaf context)
    (if (procedure? leaf)
        (get-leaf (leaf context) context)
        leaf))

  ;; If we must eval a leaf before adding a new event, the context will look wrong.
  ;; The event doesn't yet exist, so e.g. next/prev and seeding would be broken.
  ;; In this case, add an empty event to the context before evaluating.
  (define (get-leaf-early leaf time-to-add context)
    (if (procedure? leaf)
        (get-leaf leaf (context-insert context (make-event time-to-add)))
        leaf))
  )
