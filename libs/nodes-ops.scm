;; -*- geiser-scheme-implementation: chez-*-
;; ------------------------------------------------------------
;; branches
;;
;; Functions that take contexts and return new contexts.
;; These form the backbone of our system, doing all the
;; transformations on contexts that make our musical patterns.
;; The cnodes can be chained and combined with fns in @trunk.
;; The 'leaves' of our system are described in @leaf.
;; ------------------------------------------------------------
(library (nodes-ops)
  (export
    nodes-ops-docs
    in! in:
    to: to+ to- to* to/ to?
    sq: tr? cp: cp?)

  (import
    (chezscheme)
    (doc)
    (utilities) 
    (event) 
    (context)
    (node-eval)
    (nodes-subdivide)
    (nodes-chains)
    (nodes-leafs))

  ;; Adds blank events to the context.
  ;; A leaf value of 1 gives one event.
  ;; For values > 1, creates N subdivided values.
  ;; The symbol ~ creates a rest.
  (define (in! seq . ops)
    (define (impl context value)
      (let* ([value (eval-leaf-empty value (context-start context) context)]
             [num (max 1 value)]
             [dur (/ (context-length context) num)]
             [start (context-start context)]
             [make (lambda (i) (make-event-fast (+ start (* i dur)) (:sustain dur)))])
        (fold-left (lambda (c i) (context-insert c (make i))) 
                   (context-resolve context) 
                   (reverse (iota num)))))
    (apply o-> (wrap-subdivide-fn impl seq) ops))

  ;; Adds events with a single specified property
  (define (in: key seq . ops)
    (define (impl context value)
      (let ([value (eval-leaf-empty value (context-start context) context)])
        (context-insert (context-resolve context)
                        (make-event-fast (context-start context)
                                         (:sustain (context-length context))
                                         (key value)))))
    (unless (symbol? key) (error 'in: "expected :key" key))
    (if (null? ops)
        (wrap-subdivide-fn impl seq)
        (apply o-> (wrap-subdivide-fn impl seq) ops)))

  ;; A node that sets a property of events according to the pattern.
  ;; key value ... -> (context -> context)
  (define (to: . kv-pairs)
    (define (impl key)
      (lambda (context value)
        (context-map (lambda (c) (set-or-rest c value key identity))
                     (context-resolve context))))
    (build-kv kv-pairs 'to: impl))

  ;; A general 'to', taking a math op, a key and a def. The math op is
  ;; called with the current value for key and the value returned by def.
  (define (to math-op kv-pairs)
    (define (impl key)
      (lambda (context value)
        (context-map
         (lambda (c)
           (lif (current (event-get (context-event c) key #f)) current
                (set-or-rest c value key (lambda (v) (math-op current v)))
                (context-event c)))
         (context-resolve context))))
    (build-kv kv-pairs 'to-math impl))

  (define (to+ . kv-pairs) (to + kv-pairs))
  (define (to- . kv-pairs) (to - kv-pairs))
  (define (to* . kv-pairs) (to * kv-pairs))
  (define (to/ . kv-pairs) (to / kv-pairs))

  ;; A node that replaces the input with the result of applying
  ;; it to a node or pattern of nodes.
  (define (sq: seq)
    (define (impl context value)
      (let* ([value (eval-leaf value context)])
        (cond
          ((context? value) value)
          ((is-rest? value) context)
          ((procedure? value) (value context))
          (else
            (begin
              (warning 'seq "got raw value, wants procedure or ~" value)
              context)))))
    (wrap-subdivide-fn impl seq))
  
  ;;---------------------------------------------------------------
  ;; Composite chaining operators.

  ;; Same as x->, but also filters events by pred.
  ;; Confusing? People may expect this to replace only events
  ;; matching pred, and to let the others through.
  (define (tr? pred . nodes)
    (lambda (context)
      (context-filter pred ((apply x-> nodes) context))))

  ;; Same as x-> but returns both the copies and the originals.
  (define (cp: . nodes)
    (lambda (context)
      (contexts-merge ((apply x-> nodes) context)
                      (context-resolve context))))

  ;; Same as tr?, but returns both the filtered copies and the originals.
  (define (cp? pred . nodes)
    (lambda (context)
      (contexts-merge ((apply tr? pred nodes) context)
                      (context-resolve context))))

  ;; Like cp?, but merges via the predicate. Returns unaltered
  ;; those events which fail the predicate.
  (define (to? pred . nodes)
    (lambda (context)
      (let* ([resolved (context-resolve context)]
             [unfiltered (context-filter (lambda (c) (not (pred c))) resolved)])
        (contexts-merge unfiltered ((apply tr? pred nodes) context)))))

  ;;-------------------------------------------------------------------
  ;; Helper for 'to' forms above.
  (define (set-or-rest ctxt leaf key val-transform)
    (let ([val (eval-leaf leaf ctxt)])
      (cond
        ((context? val)  (error 'set-or-rest "evaluated to context" leaf))
        ((is-rest? val)  (context-event ctxt))
        (else (event-set (context-event ctxt) key (val-transform val))))))

  ;; Builds a list of transformers. Each one is built from the value of a key-value
  ;; pair, which should be an context op function, and a transform-fn, which is
  ;; built by calling impl with the key.
  (define (build-kv kv-pairs err-symbol impl)

    (define (make-subdivider key-value)
        (wrap-subdivide-fn (impl (car key-value)) (cdr key-value)))

    (let ([pairs (pairwise kv-pairs)])
      (unless pairs (error err-symbol "invalid key-value pairs" kv-pairs))

      (apply x-> (map make-subdivider pairs))))


  ;;-------------------------------------------------------------------
  (make-doc nodes-ops-docs
    (in! 
     "Adds blank events according to a repeated subdividing sequence.
An event is present where a 1 is encountered, but not where a ~ is encountered.
Numbers greater than 1 are translated into a sub-list of N elements of 1."
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
                                        (:beat 3/4 :sustain 1/4)])))

    (in:
     "Adds events according to a repeated subdividing sequence.
Each value encountered in the sequence results in an event with a 
property 'key' of that value."
     ((key :Keyword
           "The key which will be set according to values found in 'seq'")
      (seq [Any or Sequence]
           "The sequence of values and rests (~) used to generate events.")
      (ops... Function 
              "Further operators to apply to the blank events, 
              as if wrapped in 'o->'."))

     (((testp (in: :freq (over 1 [440 ~])))
          => [(:beat 0 :sustain 1/2 :freq 440)])
      ((testp (in: :freq (over 1 [50 [60 70]])))
          => [(:beat 0 :sustain 1/2 :freq 50)
              (:beat 1/2 :sustain 1/4 :freq 60)
              (:beat 3/4 :sustain 1/4 :freq 70)])))
    
    (to:
     "Sets properties on events according to a repeated subdividing sequence.
Note that arguments can be more than one pair of :key value definitions."
     ((key :Keyword
           "The key which will be set according to values found in 'seq'")
      (seq [Any or Sequence]
             "The sequence of values and rests (~) used to generate events."))

     (((testp (in! 2 (to: :amp 0.1 :freq (over 1 [440 660]))))
       => [(:beat 0  :amp 0.1 :freq 440) (:beat 0  :amp 0.1 :freq 660)]))))
  
  ) ; end module 'logic-nodes'
