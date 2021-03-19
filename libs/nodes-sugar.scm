(library (nodes-sugar)
  (export
    sy: sm: fx: g->)

  (import
    (chezscheme)
    (doc)
    (utilities)
    (samples)
    (node-eval)
    (nodes-ops)
    (nodes-subdivide)
    (nodes-chains)
    (nodes-leafs))

  (declare-keywords :inst :group :fx :control)

  ;; effects
  (define (fx: inst seq . ops)
    (unless (string? inst) 
      (error 'fx: "1st arg must be a string" inst))
    (in! seq (apply x-> (to: :fx 1 :inst inst) ops)))

  ;; synth instrument
  (define (sy: inst seq . ops)
    (unless (string? inst) 
      (error 'syn: "1st arg must be a string" inst))
    (in! seq (apply x-> (to: :inst inst) ops)))

  ;; sample instrument
  (define (sm: sample seq . ops)
    (unless (or (valid-sample? sample) (vector? sample)) 
      (error 'sm: "1st arg must be a string" sample))
    (in! seq (apply x-> (to: :inst "sampler" :smpl sample) ops)))

  (define (g-> group-num . ops)
    (apply o-> (append ops (list (to: :group group-num)))))
  
  ;;-------------------------------------------------------------------
  (make-doc nodes-sugar-docs
    (fx:
     "Adds events with the :fx flag set to true, and the :inst key set to
the first argument."
     ((seq [Number or Sequence] 
           "A Number or sequence of Numbers and rests (~).")
      (ops... Function
              "Further operators to apply to the blank events, 
              as if wrapped in 'o->'."))

     (((testp (in! (over 1 [1 ~]))) => [(:beat 0 :sustain 1/2)])
      ((testp (in! (over 1 [1 [1 1]]))) => [(:beat 0 :sustain 1/2) 
                                            (:beat 1/2 :sustain 1/4) 
                                            (:beat 3/4 :sustain 1/4)])
      ((testp (in! (over 1 [~ 2]))) => [(:beat 1/2 :sustain 1/4) 
                                        (:beat 3/4 :sustain 1/4)])))))
