#!chezscheme ;; Needed for the extra symbols like +->

(library (nodes-ops-chords)
  (export
    chord)

  (import
    (scheme)
    (harmony)
    (nodes-chains)
    (nodes-ops))

  (define (chord shape)
    (apply +-> (map (lambda (x) (apply to: :chd x)) 
                    (vector->list shape))))

  )