(library (nodes-sugar)
  (export
    syn sam fx g->)

  (import
    (chezscheme)
    (doc)
    (utilities)
    (samples)
    (node-eval)
    (nodes-ops)
    (nodes-subdivide)
    (nodes-chains)
    (nodes-continuous))

  (declare-keywords :inst :group :fx :control)

  ;; synth instrument
  (define (syn inst seq . ops)
    (unless (string? inst)
      (error 'syn "1st arg must be a string" inst))
    (in! seq (apply with (to: :inst inst) ops)))

  ;; sample instrument
  (define (sam sample seq . ops)
    (unless (or (valid-sample? sample) (vector? sample))
      (error 'sam "1st arg must be a sample or sample set" sample))
    (in! seq (apply with (to: :inst "sampler" :smpl sample) ops)))

  ;; effects
  (define (fx inst seq . ops)
    (unless (string? inst)
      (error 'fx "1st arg must be a string" inst))
    (in! seq (apply with (to: :fx 1 :inst inst) ops)))

  (define (g-> group-num . ops)
    (apply part (append ops (list (to: :group group-num)))))
  )
