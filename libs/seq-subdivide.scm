#!chezscheme ;; Needed for exporting symbols like $

;; Implements algorithms that slice a context up into chunks according
;; to several schemes, and either return a value or call an implementation
;; function for each chunk.
;; The implementation function will be retrieved from context-subdivide-fn
;; and must take a context (which will have the sliced arc) and a 'leaf'.
;; See seq-eval for information on leafs.

(library (seq-subdivide)
  (export ~ ! $
          over step
          is-rest? is-sustain?
          wrap-subdivide-fn
          sdef
          subdivide-docs)

  (import (scheme)
          (utilities)
          (doc)
          (event)
          (arc)
          (context)
          (seq-eval)
          (context-render)
          (for (seq-def) expand))

  (declare-keywords ~ $)
  (define rest-sym ~)    ;; Denotes a musical rest in a list
  (define sustain-sym $) ;; Denotes a sustained value in a list
  (tag-sdef-not-call ~ $)

  ;;-------------------------------------------------------------------
  (define-syntax over
    (syntax-rules ()
      ((_ values) (over 1 values))

      ((_ dur values)
       (make-subdivider dur (sdef values)))))

  ;;-------------------------------------------------------------------
  (define-syntax step
    (syntax-rules ()
      ((_ values) (step 1/4 values))

      ((_ slice-dur values)
       (let* ([data (sdef values)]
              [len (if (list? data) (length data) 1)])
         (make-subdivider (* slice-dur len) data)))))

  ;;-------------------------------------------------------------------
  ;; Smoothly interpolates between values in the pattern
  (define-syntax lerp
    (syntax-rules ()
      ((_ values) (lerp 1 values))

      ((_ measures values)
       (let ([data (sdef values)])
         (error 'lerp "not implemented yet")))))

  ;;-------------------------------------------------------------------
  (tag-sdef-callable over step lerp)

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
  ;; Wraps a seq in a subdividing pattern, if it hasn't already declared
  ;; that it is one. Also sets a special subdivide-fn on the input context
  ;; and restores the previous subdivide-fn on the returned context.
  (define (wrap-subdivide-fn sub-fn seq)
    (let* ([get-fn context-subdivide-fn]
           [set-fn context-with-subdivide-fn]
           [seq (if (seq-subdivider? seq) seq (make-subdivider 1 seq))])
      (lambda (context)
        (set-fn (eval-seq seq (set-fn context sub-fn)) (get-fn context)))))

  ;;-------------------------------------------------------------------
  ;; Iterates list 'vals', which is stretched over 'dur' with a
  ;; subdividing pattern according to its sublists/subsequences.
  ;;
  ;; If the context has a subdivide-fn of #f, this simply returns an
  ;; element from vals based on the time of the context's pointed
  ;; to event (or its start if empty).
  ;;
  ;; If the context has a subdivide-fn, this calls it with each slice
  ;; of the input context and an associated seq value. The
  ;; subdivide-fn must return a transformed context with the same arc.
  (define (make-subdivider dur vals)
    (when (null? vals)
      (error 'subdivide "empty subdividing sdef" vals))
    (unless (unsafe-list? vals)
      (set! vals (list vals)))

    (let* ([slice-len (/ dur (length vals))]
           [vals (map (unpack-markers slice-len) vals)])
      (define (process ctxt)
        (let ([fn (context-subdivide-fn ctxt)]
              [loop-start (round-down (context-start ctxt) dur)])
          (let loop
              ([c (make-context (context-arc ctxt))]
               [t loop-start]
               [next vals])
            (cond
             [(null? next) (loop c t vals)]
             [(>= t (context-end ctxt))
              (if (context-subdivide-fn ctxt)
                  (context-trim (rearc c (context-arc ctxt)))
                  (car next))]
             [else
              (let*-values
                  ([[num-slices next-vals] (drop-sustained next)]
                   [[item] (car next)]
                   [[next-t] (+ t (* num-slices slice-len))]
                   [[arc] (make-arc t next-t)]
                   [[subctxt] (rearc ctxt arc)])
                (cond
                 [fn (let ([s (eval-slice item fn ctxt subctxt dur)])
                       (loop (contexts-merge s c) next-t next-vals))]
                 [(within-arc? arc (context-now ctxt)) item]
                 [else (loop subctxt next-t next-vals)]))]))))
      (seq-meta-ranged #t vals process)))

  ;; Takes a value in a subdivider, an action function and
  ;; the sub-slice of a context that the two should be combined in.
  (define (eval-slice item fn ctxt subctxt dur)
    (if (seq-subdivider? item)
        (eval-sub-seq item fn ctxt subctxt dur)
        (eval-item item fn ctxt subctxt)))

  ;; Used within make-subdivider to apply subdivide-fn to item,
  ;; over a slice of time defined by subctxt. Returns subctxt filled
  ;; with new or modified values.
  (define (eval-item item subdivide-fn ctxt subctxt)
    (lif (arc (context-arc subctxt))
         (arcs-overlap? (context-arc ctxt) arc)
         (let* ([subctxt (context-to-event-after subctxt (arc-start arc))]
                [subctxt-no-subdiv (context-no-subdivide-fn subctxt)]
                [time (context-now subctxt)]
                [subdur (arc-length arc)]
                [early (eval-seq-empty item time subctxt-no-subdiv)])
           (cond
            [(is-rest? early)
             (context-resolve subctxt-no-subdiv)]
            [(unsafe-list? early)
             (eval-seq (make-subdivider subdur early) subctxt)]
            [(seq-marker? early)
             (eval-seq (unpack-marker early subdur) subctxt)]
            [else
             (subdivide-fn subctxt-no-subdiv item)]))
         subctxt))

  ;; Used within make-subdivider to evaluate a nested seq, over a
  ;; slice of time defined by subctxt. Returns subctxt filled with new
  ;; or modified values.
  ;;
  ;; The nested seq is evaluated as if it only runs during its alloted
  ;; slice of the parent seq, and is stopped otherwise. We offset the
  ;; context it is given is offset to acount for that.
  (define (eval-sub-seq item subdivide-fn ctxt subctxt dur)
    (assert (seq-subdivider? item))
    (let* ([arc (context-arc subctxt)]
           [start (arc-start arc)]
           [loop-start (round-down start dur)]
           [offset (- loop-start start)]
           [gaps (* (/ loop-start dur) (- dur (arc-length arc)))]
           [total (+ offset gaps)]
           [arc-offset (arc-math arc + total)]
           [beat-shift (lambda (b) (- b total))]
           [ev-shift (lambda (c) (event-update (context-event c) :beat beat-shift 0))]
           [subctxt (rearc subctxt arc-offset)]
           [subctxt (eval-seq item subctxt)])
      (context-trim (rearc (context-map ev-shift subctxt) arc))))

  ;; Used to unpack seq markers - see seq-def.scm.
  (define (unpack-markers dur)
    (lambda (m)
      (if (seq-marker? m) (unpack-marker m dur) m)))

  (define (unpack-marker seq-marker dur)
    (make-subdivider (/ dur (seq-marker-dur seq-marker))
                     (seq-marker-val seq-marker)))

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
          (loop (cdr lst) (add1 n))
          (values n lst))))
  )
