#!chezscheme ;; Needed for extra symbols like »

;; Implements algorithms that slice a context up into chunks according
;; to several schemes, and either return a value or call an implementation 
;; function for each chunk.
;; The implementation function will be retrieved from context-subdivide-fn 
;; and must take a context (which will have the sliced arc) and a 'leaf'.
;; See node-eval for information on leafs.

(library (nodes-subdivide)
  (export subdivide-docs
          over step
          is-rest? is-sustain?
          wrap-subdivide-fn)

  (import (scheme)
          (utilities)
          (doc)
          (event)
          (arc)
          (context)
          (node-eval)
          (for (pdef) expand))

  ;;-------------------------------------------------------------------
  (define-syntax over
    (syntax-rules ()
      ((_ values) (over 1 values))

      ((_ dur values)
       (subdivide dur (pdef values)))))

  ;;-------------------------------------------------------------------
  (define-syntax step
    (syntax-rules ()
      ((_ values) (step 1/4 values))

      ((_ slice-dur values)
       (let ([data (pdef values)])
         (subdivide (* slice-dur (length data)) data)))))

  (tag-pdef-callable over)
  (tag-pdef-callable step)

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
           [leaf (if (leaf-subdivider? leaf) leaf (subdivide 1 leaf))])
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
  (define (subdivide dur vals)

    ;; Basic checks
    (when (null? vals)
      (error 'subdivide "empty subdividing pdef" vals))
    (unless (unsafe-list? vals)
      (set! vals (list vals)))

    (let ([len (length vals)])
      (leaf-meta-ranged #t vals
       (lambda (ctxt)

         (define (call-sub-fn item subctxt subdivide-fn)
           (lif (arc (context-arc subctxt))
                (arcs-overlap? (context-arc ctxt) arc)
                (let* ([subctxt (context-to-event-after subctxt (arc-start arc))]
                       [subctxt-no-subdiv (context-no-subdivide-fn subctxt)]
                       [time (context-now subctxt)]
                       [subdur (arc-length arc)]
                       [early (eval-leaf-early item time subctxt-no-subdiv)])
                  (cond
                    ((is-rest? early)     (context-resolve subctxt-no-subdiv))
                    ((unsafe-list? early) (eval-leaf (subdivide subdur early) subctxt))
                    (else (subdivide-fn subctxt-no-subdiv item))))
                subctxt))

         (let loop ([c (make-context (context-arc ctxt))]
                    [t (round-down (context-start ctxt) dur)]
                    [next vals]
                    [prev #f])
           (cond
             ((null? next) (loop c t vals #f))
             ((>= t (context-end ctxt))
              (if (context-subdivide-fn ctxt)
                  (context-trim (rearc c (context-arc ctxt)))
                  (maybe-repeat (car next) prev)))
             (else
               (let-values ([[num-slices next-vals] (drop-stretched next)])
                 (let* ([item (maybe-repeat (car next) prev)]
                        [next-t (+ t (* num-slices (/ dur len)))]
                        [arc (make-arc t next-t)]
                        [subctxt (rearc ctxt arc)]
                        [sub-fn (context-subdivide-fn ctxt)])
                   (cond
                     (sub-fn
                      (loop (contexts-merge (call-sub-fn item subctxt sub-fn) c)
                            next-t next-vals item))

                     ((within-arc? arc (context-now ctxt)) item)
                     (else (loop subctxt next-t next-vals item))))))))))))

  ;;-----------------------------------------------------------------------
  ;; General helpers for main time-chunking routine subdivider.
  (define (maybe-repeat next last)
    (if (eq? repeat-sym next) last next))

  (define (is-rest? item)
    (eq? item rest-sym))

  (define (is-sustain? item)
    (eq? item sustain-sym))

  ;; Drops at least one value, more if the following values are sustains.
  ;; -> (values num-dropped new-lst)
  (define (drop-stretched lst)
    (let loop ([lst lst] [n 0])
      (if (and (not (null? lst))
               (or (zero? n)
                   (is-sustain? (car lst))))
          (loop (cdr lst) (+ n 1))
          (values n lst))))

  )
