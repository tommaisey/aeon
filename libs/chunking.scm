#!chezscheme ;; Needed for extra symbols like »

;; Implements algorithms that slice a context up into chunks according
;; to several schemes, and call an implementation function for each
;; chunk. The implementation function must take a context (which will
;; have the required arc) and a 'leaf', which is either a value or a
;; function that returns a value.

(library (chunking)
  (export sbdv step
          is-rest? is-sustain?
          wrap-subdivide-fn)

  (import (scheme)
          (utilities)
          (event)
          (context)
          (node-eval)
          (for (pdef) expand))

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

  (tag-pdef-callable sbdv)
  (tag-pdef-callable step)

  ;;-------------------------------------------------------------------
  ;; Puts a wrapper around a leaf that sets a certain special subdivide-fn
  ;; on the context, then unsets it on the returned context.
  ;; This also wraps the leaf in a subdividing pattern, if it hasn't
  ;; already declared that it is one.
  (define (wrap-subdivide-fn fn leaf)
    (let* ([get-fn context-subdivide-fn]
           [set-fn context-with-subdivide-fn]
           [leaf (if (leaf-subdivider? leaf) leaf (subdivide 1 leaf))])
      (lambda (context)
        (set-fn (eval-leaf leaf (set-fn context fn)) (get-fn context)))))

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
  ;; If the context has a subdivide-fn of #f, this simply returns a value from vals
  ;; based on the context's pointed to event, or the context's start if empty.
  ;; If the context has a subdivide-fn, it calls it for each slice with the same
  ;; context except with its arc changed to represent the slice. The subdivide-fn 
  ;; must return a subdivideed context with the same arc.
  (define (subdivide dur vals)

    ;; Basic checks
    (when (null? vals)
      (error 'subdivide "empty subdividing pdef" vals))
    (unless (unsafe-list? vals)
      (set! vals (list vals)))

    (let ([len (length vals)])
      (leaf-meta-ranged #t vals
       (lambda (ctxt)

         (define (transform item subctxt subdivide-fn)
           (lif (arc (context-arc subctxt))
                (arcs-overlap? (context-arc ctxt) arc)
                (let* ([subctxt (context-to-event-after subctxt (arc-start arc))]
                       [sans-tran (context-no-subdivide-fn subctxt)]
                       [time (context-now subctxt)]
                       [dur (arc-length arc)]
                       [early (eval-leaf-early item time sans-tran)])
                  (cond
                    ((is-rest? early)     (context-resolve sans-tran))
                    ((unsafe-list? early) (eval-leaf (subdivide dur early) subctxt))
                    (else (subdivide-fn sans-tran item))))
                subctxt))

         (let loop ([c (make-context (context-arc ctxt))]
                    [t (round-down-f (context-start ctxt) dur)]
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
                        [tfn (context-subdivide-fn ctxt)])
                   (cond
                     (tfn
                      (loop (contexts-merge (transform item subctxt tfn) c)
                            next-t next-vals item))

                     ((within-arc? arc (context-now ctxt)) item)
                     (else (loop subctxt next-t next-vals item))))))))))))

  )
