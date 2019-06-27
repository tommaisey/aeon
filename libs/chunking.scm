#!chezscheme ;; Needed for extra symbols like »

;; Implements algorithms that slice a context up into chunks according
;; to several schemes, and call an implementation function for each
;; chunk. The implementation function must take a context (which will
;; have the required arc) and a 'leaf', which is either a value or a
;; function that returns a value.

(library (chunking)
  (export sbdv step
          is-rest? is-sustain?
          wrap-transform-fn
          pattern-error)

  (import (scheme)
          (utilities)
          (event)
          (context)
          (node-eval)
          (for (pdef) expand))

  (define (pattern-error name type val)
    (format "pattern error: got '~A', ~A expects a ~A" val name type))

  ;;-------------------------------------------------------------------
  (define-syntax sbdv
    (syntax-rules ()
      ((_ def) (sbdv 1 def))

      ((_ dur def)
       (subdivide dur (make-pdef-data def)))))

  (define-syntax step
    (syntax-rules ()
      ((_ def) (step 1/4 def))

      ((_ slice-dur def)
       (let* ([data (make-pdef-data def)])
         (subdivide (* slice-dur (length data)) data)))))

  ;;-------------------------------------------------------------------
  ;; Puts a wrapper around a leaf that sets a certain special transform-fn on
  ;; the context, then unsets it on the returned context.
  ;; This also tests if the leaf returns values or contexts, and if it
  ;; returns values, it wraps it in a subdividing pattern so it works
  ;; in places that expect to use context-transform-fn.
  (define (wrap-transform-fn fn leaf)
    (let* ([get context-transform-fn]
           [set context-with-transform-fn]
           [fake-ctxt (set (make-empty-context 0 1) (lambda (c v) c))]
           [leaf (if (context? (eval-leaf leaf fake-ctxt)) leaf (sbdv [leaf]))])
      (lambda (context)
        (set (eval-leaf leaf (set context fn)) (get context)))))

  ;;-----------------------------------------------------------------------
  ;; General helpers for main time-chunking routines like subdivider.

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

  ;;--------------------------------------------------------------------------------
  ;; Iterates list 'vals', which is stretched over 'dur' with a subdividing pattern
  ;; according to its sublists.
  ;;
  ;; If the context has a transform-fn of #f, this simply returns a value from vals based
  ;; on the context's pointed to event, or the context's start if empty.
  ;; If the context has a transform-fn, it calls it for each slice with the same context
  ;; except with its arc changed to represent the slice. The transform-fn must return
  ;; a transformed context with the same arc.
  (define (subdivide dur vals)

    ;; Basic checks
    (when (null? vals)
      (error 'subdivide "empty subdividing pdef" vals))
    (unless (unsafe-list? vals)
      (set! vals (list vals)))

    (let ([len (length vals)])
      (lambda (ctxt)

        (define (transform item subctxt transform-fn)
          (lif (arc (context-arc subctxt))
               (arcs-overlap? (context-arc ctxt) arc)
               (let* ([subctxt (context-to-event-after subctxt (arc-start arc))]
                      [eval-d (eval-leaf-early item (context-now subctxt) subctxt)]
                      [dur (arc-length arc)])
                 (cond
                   ((is-rest? eval-d)     (context-resolve subctxt))
                   ((unsafe-list? eval-d) ((subdivide dur eval-d) subctxt))
                   (else                  (transform-fn subctxt item))))
               subctxt))

        (let loop ([c (make-context (context-arc ctxt))]
                   [t (round-down-f (context-start ctxt) dur)]
                   [next vals]
                   [prev #f])
          (cond
            ((>= t (context-end ctxt)) (context-trim (rearc c (context-arc ctxt))))
            ((null? next) (loop c t vals #f))
            (else
              (let-values ([[num-slices next-vals] (drop-stretched next)])
                (let* ([transform-fn (context-transform-fn ctxt)]
                       [item (maybe-repeat (car next) prev)]
                       [next-t (+ t (* num-slices (/ dur len)))]
                       [arc (make-arc t next-t)]
                       [subctxt (rearc ctxt arc)])
                  (cond
                    (transform-fn
                     (loop (contexts-merge (transform item subctxt transform-fn) c)
                            next-t next-vals item))

                    ((within-arc? arc (context-now ctxt)) item)
                    (else (loop subctxt next-t next-vals item)))))))))))

  )
