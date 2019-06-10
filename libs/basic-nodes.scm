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
(library (basic-nodes)
  (export
    sbdv step in! in:
    to: to+ to- to* to/ to?
    rp: tr? cp: cp?)

  (import
    (chezscheme) (srfi s26 cut)
    (utilities) (event) (context)
    (chunking)
    (node-eval)
    (chain-nodes)
    (value-nodes))

  ;; A node that adds blank events according to a subdividing pattern.
  (define (in! pdef . ops)
    (apply o-> (lambda (context) (dispatch-pdef pdef context in!impl)) ops))

  ;; A node that adds events with a single specified property.
  (define (in: key pdef . ops)
    (apply o-> (lambda (context) (dispatch-pdef pdef context (in:impl key))) ops))

  ;; A node that replaces the input with the result of applying
  ;; it to each pattern member, which must all be functional nodes.
  (define (rp: pdef)
    (lambda (context)
      (dispatch-pdef pdef context rp:impl)))

  ;; A node that sets a property of events according to the pattern.
  ;; key value ... -> (context -> context)
  (define (to: . kv-pairs)
    (apply x-> (kv-pairs-to-nodes kv-pairs to:impl)))

  ;; A general 'to', taking a math op, a key and a def. The math op is
  ;; called with the current value for key and the value returned by def.
  (define (to math-op . kv-pairs)
    (define (impl key) (to-math-impl math-op key))
    (apply x-> (kv-pairs-to-nodes kv-pairs impl)))

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
  ;; Implementation functions to be supplied to a chunking algorithm.
  ;; These must return a list of events, not a context.

  (define (rp:impl context leaf)
    (let ([result (get-leaf leaf context)])
      (cond
        ((is-rest? result) (context-events-next (context-resolve context)))
        ((not (context? result)) (error 'rp: "expects procedure" result))
        (else (context-events-next result)))))

  ;; Helper for 'in' impls to get a value from a leaf node,
  ;; and to use it to add a new note/notes to the context.
  (define (make-events maker context leaf performer)
    (let* ([c context]
           [val (get-leaf-early leaf (context-start c) c)])
      (cond
        ((is-rest? val) (list))
        ((context? val) (context-events-next val))
        ((unsafe-list? val)
         (subdivider c (context-length c) val performer))
        (else (maker val)))))

  ;; Adds blank events to the context with a subdividing pattern.
  ;; A leaf value of 1 gives one event.
  ;; For values > 1, creates N subdivided values.
  ;; The symbol ~ creates a rest.
  (define (in!impl context leaf)
    (make-events
     (lambda (val)
       (let* ([num (max 1 val)]
              [dur (/ (context-length context) num)]
              [start (context-start context)]
              [make (lambda (i) (make-event (+ start (* i dur))
                                            (:sustain dur)))])
         (map make (iota num))))
     context leaf in!impl))

  ;; Adds events with a property defined by 'key'.
  (define (in:impl key)
    (lambda (context leaf)
      (make-events
       (lambda (val)
         (let ([dur (context-length context)]
               [start (context-start context)])
           (list (make-event start (:sustain dur) (key val)))))
       context leaf (in:impl key))))

  ;; Used in above in forms.
  (define :sustain ':sustain)

  ;; Helper for 'to' forms below.
  (define (set-or-rest c leaf key val-transform)
    (let ([val (get-leaf leaf c)])
      (if (is-rest? val)
          (context-event c)
          (event-set (context-event c) key (val-transform val)))))

  ;; Sets the property 'key' on all events in the context.
  (define (to:impl key)
    (lambda (context leaf)
      (define (map-fn c)
        (set-or-rest c leaf key (lambda (v) v)))
      (context-events-next (context-map map-fn (context-resolve context)))))

  ;; Sets the property 'key' by doing some math on the old
  ;; value together with leaf.
  (define (to-math-impl math-fn key)
    (lambda (context leaf)
      (define (map-fn c)
        (let ([current (event-get (context-event c) key #f)])
          (if current
              (set-or-rest c leaf key (lambda (v) (math-fn current v)))
              (context-event c))))
      (context-events-next (context-map map-fn (context-resolve context)))))

  ;;-------------------------------------------------------------------
  ;; Helper for building a list of nodes from pairs of keys and values,
  ;; used in the implementation of the to: family of ops above.
  ;; (key value ...), (key -> impl) -> ((context -> context) ...)
  (define (kv-pairs-to-nodes pairs impl)
    (define (make-node key val)
      (lambda (c) (dispatch-pdef val c (impl key))))
    (if (zero? (mod (length pairs) 2))
        (let loop ([pairs pairs] [out '()])
          (if (null? pairs)
              (reverse out)
              (loop (cddr pairs)
                    (cons (make-node (car pairs) (cadr pairs)) out))))
        (raise (string-append
                "'to' family operators must have an even number of arguments, "
                "made up of alternating keys and patterns."))))

  ) ; end module 'logic-nodes'
