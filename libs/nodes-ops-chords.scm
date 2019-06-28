#!chezscheme ;; Needed for the extra symbols like +->

(library (nodes-ops-chords)
  (export chord)

  (import
    (scheme)
    (harmony)
    (node-eval)
    (nodes-chains)
    (nodes-subdivide)
    (nodes-ops))

  ;; TODO: This implementation is very slow.
  ;; It lets you use a pattern for 'shape', however,
  ;; which is mighty useful. Find a more efficient way.
  (define (chord shape)
    (define (impl context val)
        (let ([vec (eval-leaf val context)])
          (when (not (vector? vec))
            (error 'chord "expected vector" vec))
          ((apply +-> (map (lambda (x) (to: :chd x))
                          (vector->list vec)))
           context)))
    (wrap-subdivide-fn impl shape))

  )