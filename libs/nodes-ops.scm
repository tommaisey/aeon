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
    in! in:
    to: to+ to- to* to/ to?
    rp: tr? cp: cp?)

  (import
    (chezscheme)
    (utilities) (event) (context)
    (node-eval)
    (nodes-subdivide)
    (nodes-chains)
    (nodes-leafs))

  ;; Adds blank events to the context.
  ;; A leaf value of 1 gives one event.
  ;; For values > 1, creates N subdivided values.
  ;; The symbol ~ creates a rest.
  (define (in! leaf . ops)
    (define (impl context value)
      (let* ([value (eval-leaf-early value (context-start context) context)]
             [num (max 1 value)]
             [dur (/ (context-length context) num)]
             [start (context-start context)]
             [make (lambda (i) (make-event (+ start (* i dur))
                                           (:sustain dur)))])
        (fold-left (lambda (c i) (context-insert c (make i))) 
                   (context-resolve context) 
                   (reverse (iota num)))))
    (apply o-> (wrap-subdivide-fn impl leaf) ops))

  ;; Adds events with a single specified property
  (define (in: key leaf . ops)
    (define (impl context value)
      (let ([value (eval-leaf-early value (context-start context) context)])
        (context-insert (context-resolve context)
                        (make-event (context-start context)
                                    (:sustain (context-length context))
                                    (key value)))))
    (unless (symbol? key) (error 'in "expected a :key" key))
    (apply o-> (wrap-subdivide-fn impl leaf) ops))

  ;; A node that replaces the input with the result of applying
  ;; it to a node or pattern of nodes.
  (define (rp: leaf)
    (define (impl context value)
      (let* ([context (context-resolve context)]
             [value (eval-leaf-early value (context-start context) context)])
        (if (procedure? value)
            (value context)
            (begin
              (warning 'rp: "got raw value, wants procedure" value)
              context))))
    (wrap-subdivide-fn impl leaf))

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
  (define (to math-op . kv-pairs)
    (define (impl key)
      (lambda (context value)
        (context-map
         (lambda (c)
           (lif (current (event-get (context-event c) key #f)) current
                (set-or-rest c value key (lambda (v) (math-op current v)))
                (context-event c)))
         (context-resolve context))))
    (build-kv kv-pairs 'to-math impl))

  (define (to+ . kv-pairs) (apply to + kv-pairs))
  (define (to- . kv-pairs) (apply to - kv-pairs))
  (define (to* . kv-pairs) (apply to * kv-pairs))
  (define (to/ . kv-pairs) (apply to / kv-pairs))

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
  ;; Helper functions
  (define :sustain ':sustain) ;; Needed in above in forms.

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

  ) ; end module 'logic-nodes'
