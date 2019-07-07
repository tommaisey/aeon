#!chezscheme ;; Needed for the extra symbols like +->

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
    (nodes-ops))

  ;; Originally this was a 3 liner using +-> and to: but that turned out
  ;; to be slow. Perhaps too many levels of indirection? Hand-rolled instead.
  (define (chord shape-pattern)
    (define (cmap degrees)
      (lambda (ctxt)
        (map (lambda (d) (event-set (context-event ctxt) :chd d)) degrees)))
    (define (impl context val)
      (let ([vec (eval-leaf val context)])
        (cond
         ((is-rest? vec) (context-resolve context))
         ((not (vector? vec)) (error 'chord "expected vector" vec))
         (else (context-map (cmap (vector->list vec))
                            (context-resolve context)
                             append)))))
    (wrap-subdivide-fn impl shape-pattern))

  )