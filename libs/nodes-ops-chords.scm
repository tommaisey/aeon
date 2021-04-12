(library (nodes-ops-chords)
  (export chord)

  (import
    (scheme)
    (harmony)
    (event)
    (context)
    (node-eval)
    (nodes-chains)
    (nodes-subdivide)
    (nodes-ops)
    (only (matchable) match ?))

  ;; Originally this was a 3 liner using 'with' and 'to:' but
  ;; that turned out to be slow.
  ;; Too many levels of indirection?
  ;; Hand-rolled instead.
  (define (chord shape-pattern)
    (define (cmap degrees)
      (lambda (ctxt)
        (map (lambda (d) (event-set (context-event ctxt) :chd d)) degrees)))
    (define (impl context val)
      (match (eval-seq val context)
        [(? is-rest?) (context-resolve context)]
        [(? vector? v) (context-map (cmap (vector->list v))
                                    (context-resolve context)
                                    append)]
        [v (error 'chord "expected vector" v)]))
    (wrap-subdivide-fn impl shape-pattern))

  )
