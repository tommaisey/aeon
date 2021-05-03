(library (seq-eval)
  (export
    eval-seq
    eval-seq-empty
    seq-meta-ranged
    seq-subdivider?
    seq-meta-field
    seq-meta-rng-max
    seq-meta-rng-min)

  (import (scheme) (utilities) (matchable) (arc) (event) (context))

  ;;--------------------------------------------------------------
  ;; Get a value from a seq. The source seq might be a plain value, a
  ;; naked contextual function or a 'decorated' seq-meta object.
  (define (eval-seq seq context)
    (cond
      [(procedure? seq) (eval-seq (seq context) context)]
      [(seq-meta? seq) (eval-seq (seq-meta-fn seq) context)]
      [else seq]))

  ;; If we eval a seq in order to add a new event, the context will look
  ;; wrong - the event doesn't yet exist, but some functions relt on that.
  ;; So we add an empty event to the context before evaluating.
  ;; TODO: is this needed? rand seeding doesn't use the event any more.
  ;;       are there other functions that need this?
  (define (eval-seq-empty seq time-to-add context)
    (if (or (procedure? seq) (seq-meta? seq))
        (eval-seq seq (context-insert context (make-event time-to-add)))
        seq))

  ;;-------------------------------------------------------------------
  ;; An object containing a seq function as well as some metadata
  ;; that help to evaluate the tree correctly in some situations.
  ;;
  ;; The metadata contains the maximum and minimum values that the
  ;; seq can produce, if this can be ascertained. This can be used
  ;; to optimise evaluation of the graph in some cases - in others
  ;; it's impossible to evaluate the graph without it (e.g. when
  ;; the seq is doing a transformation of events in time).
  ;;
  ;; There's also a tag describing whether the seq function is a
  ;; subdivider, which influences how it will be evaluated in some
  ;; cases (e.g. nested subdividers).
  ;;
  ;; A record-writer for this is defined at the bottom of the file.
  (define-record-type seq-meta
    (fields (immutable rng-min) ;; #f if impossible to detect
            (immutable rng-max) ;; #f if impossible to detect
            (immutable fn)
            (immutable subdivider?)))

  ;; Try to create a seq object, filling in the min and max possible values
  ;; from an input list. Input is filtered for symbols, so ~ rests are ok.
  ;; If we can't work out a min & max value from lst, just returns the raw fn.
  (define seq-meta-ranged
    (case-lambda
      ((lst fn) (seq-meta-ranged #f lst fn))

      ((subdivider? lst fn)
       (let* ([lst (filter (lambda (x) (or (number? x) (seq-meta? x))) lst)]
              [range-min (seq-foldl seq-meta-rng-min min +inf.0 lst)]
              [range-max (seq-foldl seq-meta-rng-max max -inf.0 lst)])
         (cond
           [(and range-min range-max)
            (make-seq-meta range-min range-max fn subdivider?)]
           [subdivider?
            (make-seq-meta #f #f fn subdivider?)]
           [else fn])))))

  (define (seq-subdivider? seq)
    (and (seq-meta? seq)
         (seq-meta-subdivider? seq)))

  ;; Tries to get field from a seq - if it's an undecorated procedure
  ;; we return false. Otherwise we return a primitive value.
  (define (seq-meta-field v seq-field)
    (cond
      ((or (eq? v #f)        ;; propagate failure
           (procedure? v)    ;; undecorated fn, can't get metadata
           (unsafe-list? v)) ;; a list, can't get metadata
       #f)
      ((seq-meta? v) (seq-field v))
      (else v)))

  ;; Applies a reduce on a mixed list of primitive values, contextual
  ;; functions and seq objects. Helpful in constructing seq objects.
  (define (seq-foldl seq-field fn start-val values)
    (define (combine result v)
      (lif [meta (seq-meta-field v seq-field)]
           (and result meta)
           (fn result meta) #f))
    (fold-left combine start-val values))

  ;; This has to be after all definitions.
  (record-writer
   (type-descriptor seq-meta)
   (lambda (rec p wr)
     (display "#[seq" p)
     (when (seq-meta-subdivider? rec)
       (display " subdiv" p))
     (when (seq-meta-rng-min rec)
       (display " hi:" p)
       (display (number->string (seq-meta-rng-min rec)) p)
       (display " lo:" p)
       (display (number->string (seq-meta-rng-max rec)) p)
       (display "" p))
     (display "]" p)))
  )
