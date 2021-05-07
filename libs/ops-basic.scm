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
(library (ops-basic)
  (export
    ops-basic-docs
    in! in:
    to: to+ to- to* to/ to?
    sq: tr? cp: cp?)

  (import
    (chezscheme)
    (doc)
    (utilities)
    (event)
    (context)
    (seq-eval)
    (seq-subdivide)
    (seq-continuous)
    (ops-chains)
    (context-render))

  ;; Adds events to the context with property 'key' initialised using seq.
  ;; The remaining ops can be a mixed list of :key value
  ;; pairs and individual transformer functions.
  (define (in: key seq . ops)
    (define (impl context value)
      (let ([value (eval-seq-empty value (context-start context) context)])
        (context-insert (context-resolve context)
                        (make-event-fast (context-start context)
                                         (:sustain (context-length context))
                                         (key value)))))
    (unless (symbol? key)
      (error 'in: "expected :key" key))
    (lif (event-maker (wrap-subdivide-fn impl seq))
         (null? ops) event-maker
         (part event-maker (to-generic 'in: ops))))

  ;; Adds blank events to the context using seq.
  ;; The symbol ~ creates a rest.
  ;; The remaining ops can be a mixed list of :key value
  ;; pairs and individual transformer functions.
  (define (in! seq . ops)
    (define (impl context value)
      (let ([value (eval-seq-empty value (context-start context) context)])
        (context-insert (context-resolve context)
                        (make-event-fast (context-start context)
                                         (:sustain (context-length context))))))
    (lif (event-maker (wrap-subdivide-fn impl seq))
         (null? ops) event-maker
         (part event-maker (to-generic 'in! ops))))

  ;; A node that sets a property of events according to the pattern.
  ;; key value ... -> (context -> context)
  (define (to: . ops)
    (to-generic 'to: ops))

  ;; Mathematical versions of to that modify existing properties.
  (define (to+ . ops) (to-math + 'to+ ops))
  (define (to- . ops) (to-math - 'to- ops))
  (define (to* . ops) (to-math * 'to* ops))
  (define (to/ . ops) (to-math / 'to/ ops))

  ;; A node that replaces the input with the result of applying
  ;; it to a node or pattern of nodes.
  (define (sq: seq)
    (define (impl context value)
      (let* ([value (eval-seq value context)])
        (cond
          ((context? value) value)
          ((is-rest? value) context)
          ((procedure? value) (value context))
          (else
            (begin
              (warning 'sq: "got raw value, wants procedure or ~" value)
              context)))))
    (wrap-subdivide-fn impl seq))

  ;;---------------------------------------------------------------
  ;; Composite chaining operators.

  ;; Same as 'with', but also filters events by pred.
  ;; Confusing? People may expect this to replace only events
  ;; matching pred, and to let the others through.
  (define (tr? pred . nodes)
    (let ([f (apply with nodes)])
      (lambda (context)
        (context-filter pred (f context)))))

  ;; Same as 'with' but returns both the copies and the originals.
  (define (cp: . nodes)
    (let ([f (apply with nodes)])
      (lambda (context)
        (contexts-merge (f context) (context-resolve context)))))

  ;; Same as tr?, but returns both the filtered copies and the originals.
  (define (cp? pred . nodes)
    (let ([f (apply tr? pred nodes)])
      (lambda (context)
        (contexts-merge (f context) (context-resolve context)))))

  ;; Like cp?, but merges via the predicate. Returns unaltered
  ;; those events which fail the predicate.
  (define (to? pred . nodes)
    (let ([f (apply tr? pred nodes)])
      (lambda (context)
        (let* ([resolved (context-resolve context)]
               [unfiltered (context-filter (lambda (c) (not (pred c))) resolved)])
          (contexts-merge unfiltered (f context))))))

  ;;-------------------------------------------------------------------
  ;; Helper for 'to' forms above.
  (define (set-or-rest ctxt seq key val-transform)
    (let ([val (eval-seq seq ctxt)])
      (cond
        ((context? val)  (error 'set-or-rest "evaluated to context" seq))
        ((is-rest? val)  (context-event ctxt))
        (else (event-set (context-event ctxt) key (val-transform val))))))

  ;; Builds a list of transformers from a mixed flat list of key-value
  ;; pairs and non-key-value raw transformers. Abstractly:
  ;; (:key1 val1 non-kv-transformer :key3 val3)
  ;;   => ((to: key1 val1) non-kv-transformer (to: key3 val3))
  (define (extract-kvs err-symbol ops make-impl)
    (let* ([test-ctxt (make-empty-context 0 1)]
           [value-seq? (lambda (seq) (not (context? (eval-seq seq test-ctxt))))])
      (let loop ([result '()] [src ops])
        (if (null? src)
            (reverse result)
            (let ([head (car src)] [tail (cdr src)])
              (cond
               ((not (symbol? head))
                (when (value-seq? head)
                  (error err-symbol "seq in place of transformer" ops))
                (loop (cons head result) tail))
               ((null? tail)
                (error err-symbol ":key followed by no seq" head))
               (else
                (let* ([seq (car tail)]
                       [impl (wrap-subdivide-fn (make-impl head) seq)])
                  (unless (value-seq? seq)
                    (error err-symbol "non-seq for :key" head))
                  (loop (cons impl result) (cdr tail))))))))))

  ;; Implementations used in several forms above
  (define (to-generic err-sym ops)
    (define (make-impl key)
      (lambda (context value)
        (context-map (lambda (c) (set-or-rest c value key identity))
                     (context-resolve context))))
    (apply with (extract-kvs err-sym ops make-impl)))

  (define (to-math math-op op-sym ops)
    (define (make-impl key)
      (lambda (context value)
        (context-map
         (lambda (c)
           (lest (current (event-get (context-event c) key #f))
                 (set-or-rest c value key (lambda (v) (math-op current v)))
                 (context-event c)))
         (context-resolve context))))
    (apply with (extract-kvs op-sym ops make-impl)))

  ;;-------------------------------------------------------------------
  (make-doc ops-basic-docs
    (in!
     "Adds blank events according to a repeated subdividing sequence.
An event is present where a 1 is encountered, but not where a ~ is encountered.
Numbers greater than 1 are translated into a sub-list of N elements of 1."
     ((seq [Number or Sequence]
           "A Number or sequence of Numbers and rests (~).")
      (ops... Function
              "Further operators to apply to the blank events,
              as if wrapped in 'part'."))

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
              as if wrapped in 'part'."))

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
