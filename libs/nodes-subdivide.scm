#!chezscheme ;; Needed for extra symbols like »

;; Implements algorithms that slice a context up into chunks according
;; to several schemes, and either return a value or call an implementation
;; function for each chunk.
;; The implementation function will be retrieved from context-subdivide-fn
;; and must take a context (which will have the sliced arc) and a 'leaf'.
;; See node-eval for information on leafs.

(library (nodes-subdivide)
  (export ~ ! $
          over step
          is-rest? is-sustain?
          wrap-subdivide-fn
          pdef
          subdivide-docs)

  (import (scheme)
          (utilities)
          (doc)
          (event)
          (arc)
          (context)
          (node-eval)
          (for (pdef) expand))

  (declare-keywords ~ $)
  (define rest-sym ~)    ;; Denotes a musical rest in a list
  (define sustain-sym $) ;; Denotes a sustained value in a list
  (tag-pdef-not-call ~ $)

  ;;-------------------------------------------------------------------
  (define-syntax over
    (syntax-rules ()
      ((_ values) (over 1 values))

      ((_ dur values)
       (make-subdivider dur (pdef values)))))

  ;;-------------------------------------------------------------------
  (define-syntax step
    (syntax-rules ()
      ((_ values) (step 1/4 values))

      ((_ slice-dur values)
       (let* ([data (pdef values)]
              [len (if (list? data) (length data) 1)])
         (make-subdivider (* slice-dur len) data)))))

  ;;-------------------------------------------------------------------
  ;; Smoothly interpolates between values in the pattern
  (define-syntax lerp
    (syntax-rules ()
      ((_ values) (lerp 1 values))

      ((_ measures values)
       (let ([data (pdef values)])
         (error 'lerp "not implemented yet")))))

  ;;-------------------------------------------------------------------
  (tag-pdef-callable over step lerp)

  ;;-------------------------------------------------------------------
  (make-doc subdivide-docs
    (over
     "Creates a repeated sequence of values, spread evenly over the time period 'dur'.
Sub-lists further subdivide the step they occupy, according to the rules of 'over'."
     ((dur [/opt Number 1]
           "The length of time to spread the pattern over")
      (values [/list [Number or Function]]
              "The values to be distributed over period 'dur'"))

     (((over [1 2 3]) => "repeating sequence of [1 2 3] each 1 measure")
      ((over 2 [1 2 3]) => "repeating sequence of [1 2 3] every 2 measures")))

    (step
     "Creates a repeated sequence of values, with each value applying for 'slice-dur'.
The total length of the sequence is therefore (* slice-dur (length def)).
Sub-lists further subdivide the step they occupy, according to the rules of 'over'."
     ((slice-dur [/opt Number 1/4]
                 "The length of time each value will apply for")
      (values [/list [Number or Function]]
              "The values to be distributed, one per period 'slice-dur'"))

     (((step [1 2 3]) => "repeating sequence of [1 2 3] each 3/4 measure")
      ((step 1/3 [1 2 3]) => "repeating sequence of [1 2 3] each 1 measure"))))

  ;;-------------------------------------------------------------------
  ;; Wraps a leaf in a subdividing pattern, if it hasn't already declared
  ;; that it is one. Also sets a special subdivide-fn on the input context
  ;; and restores the previous subdivide-fn on the returned context.
  (define (wrap-subdivide-fn sub-fn leaf)
    (let* ([get-fn context-subdivide-fn]
           [set-fn context-with-subdivide-fn]
           [leaf (if (leaf-subdivider? leaf) leaf (make-subdivider 1 leaf))])
      (lambda (context)
        (set-fn (eval-leaf leaf (set-fn context sub-fn)) (get-fn context)))))

  ;;----------------------------------------------------------------------------
  ;; Iterates list 'vals', which is stretched over 'dur' with a subdividing
  ;; pattern according to its sublists.
  ;;
  ;; If the context has a subdivide-fn of #f, this simply returns an element of
  ;; from vals based on the time of the context's pointed to event (or its start
  ;; if empty).
  ;; If the context has a subdivide-fn, this calls it with each slice of the
  ;; input context and an associated leaf value. The subdivide-fn must return
  ;; a transformed context with the same arc.
  (define (make-subdivider dur vals)
    (when (null? vals)
      (error 'subdivide "empty subdividing pdef" vals))
    (unless (unsafe-list? vals)
      (set! vals (list vals)))

    (let ([slice-len (/ dur (length vals))])
      (define (process ctxt)
        (let loop ([c (make-context (context-arc ctxt))]
                   [t (round-down (context-start ctxt) dur)]
                   [next vals]
                   [prev #f])
          (cond
           [(null? next) (loop c t vals #f)]
           [(>= t (context-end ctxt))
            (if (context-subdivide-fn ctxt)
                (context-trim (rearc c (context-arc ctxt)))
                (car next))]
           [else
            (let-values ([[num-slices next-vals] (drop-sustained next)])
              (let* ([item (car next)]
                     [next-t (+ t (* num-slices slice-len))]
                     [arc (make-arc t next-t)]
                     [subctxt (rearc ctxt arc)]
                     [sub-fn (context-subdivide-fn ctxt)])
                (cond
                 [sub-fn (let* ([s (eval-slice item sub-fn ctxt subctxt)]
                                [c (contexts-merge s c)])
                           (loop c next-t next-vals item))]
                 [(within-arc? arc (context-now ctxt)) item]
                 [else (loop subctxt next-t next-vals item)])))])))
      (leaf-meta-ranged #t vals process)))

  ;; Used within the above function to apply subdivide-fn to item,
  ;; over a slice of time defined by subctxt. Returns subctxt filled
  ;; with new or modified values, which are created by subdivide.
  (define (eval-slice item subdivide-fn ctxt subctxt)
    (lif (arc (context-arc subctxt))
         (arcs-overlap? (context-arc ctxt) arc)
         (let* ([subctxt (context-to-event-after subctxt (arc-start arc))]
                [subctxt-no-subdiv (context-no-subdivide-fn subctxt)]
                [time (context-now subctxt)]
                [subdur (arc-length arc)]
                [early (eval-leaf-empty item time subctxt-no-subdiv)])
           (cond
            [(is-rest? early)     (context-resolve subctxt-no-subdiv)]
            [(unsafe-list? early) (eval-leaf (make-subdivider subdur early) subctxt)]
            [else (subdivide-fn subctxt-no-subdiv item)]))
         subctxt))

  ;;-----------------------------------------------------------------------
  ;; General helpers for main time-chunking routine subdivider.
  (define (is-rest? item)
    (eq? item rest-sym))

  (define (is-sustain? item)
    (eq? item sustain-sym))

  ;; Drops at least one value, more if the following values are sustains.
  ;; -> (values num-dropped new-lst)
  (define (drop-sustained lst)
    (let loop ([lst lst] [n 0])
      (if (and (not (null? lst))
               (or (zero? n)
                   (is-sustain? (car lst))))
          (loop (cdr lst) (+ n 1))
          (values n lst))))

  ;;-------------------------------------------------------------------
  ;; Returns the previous value and the next value for a given time.
  ;; The 'previous' value may in fact land exactly on 'time', in which
  ;; case the 'next' value should be exactly (/ dur (length pdef)) away.
  ;; => (values prev-val prev-time next-val next-time)
  (define (lerp-values pdef dur time)
    (when (not (unsafe-list? pdef))
      (error 'values-at-time "non-list pdef" pdef))
    (when (null? pdef)
      (error 'values-at-time "empty pdef"))
    (let ([start (* dur (floor (/ time dur)))])
      (let loop ([p pdef]
                 [t0 start]
                 [t1 (- time start)]
                 [step (/ dur (length pdef))]
                 [1st (car pdef)])
        (if (>= t1 step)
            (loop (cdr p) (+ t0 step) (- t1 step) step 1st)
            (let ([v0 (car p)] [v1 (if (null? (cdr p)) 1st (cadr p))])
              (if (pair? v0)
                  (loop v0 t0 t1 (/ step (length v0)) v1)
                  (let ([v1 (if (pair? v1) (car v1) v1)])
                    (values v0 t0 v1 (+ t0 step)))))))))

  )
