;; -*- geiser-scheme-implementation: chez-*-
(library (context)
  (export
    testc
    context
    context?
    make-context
    make-empty-context
    context-event
    context-events-next
    context-events-prev
    context-arc
    context-start
    context-end
    context-now
    context-print
    context-insert
    context-with-events
    context-clear-events
    context-with-arc rearc
    context-with-chain
    context-append-chain
    context-pop-chain
    context-resolve
    context-length
    context-move
    context-to-closest-event
    context-to-event-after
    context-rewind
    context-map
    context-filter
    context-trim
    context-sort
    context-empty?
    contexts-merge)
  (import (chezscheme) (utilities) (event))

  ;; ---------------------------------------------
  ;; A context for a event in a series. events-next contains the
  ;; remaining events (starting with 'this' event) and prev-events
  ;; contains the previous events in reverse, starting with the one
  ;; preceding 'this'.
  ;;
  ;; The 'chain' is a list of context-lambdas. Calling the first one
  ;; should render the context - it does so by calling the second one,
  ;; etc. This 'inversion of control' allows each of the lambdas to call
  ;; their ancestors with different time chunks, to acheive lookahead.
  (define-record-type context
    (fields (immutable events-next)
            (immutable events-prev)
            (immutable arc)
            (immutable chain))
    (protocol
     (lambda (new)
       (case-lambda ; events-prev is optional in ctor
         ((events-next events-prev arc chain)
          (new events-next events-prev arc chain))
         ((events-next arc)
          (new events-next '() arc '()))
         ((arc)
          (new '() '() arc '()))))))

  (define (make-empty-context start end)
    (make-context (make-arc start end)))

  (define testc
    (case-lambda
      ((pattern) (testc pattern 0 1))
      ((pattern start end) (pattern (make-empty-context start end)))))

  (define (context-event c)
    (lif (n (context-events-next c))
         (null? n) '() (car n)))

  (define (context-start c) (arc-start (context-arc c)))
  (define (context-end c) (arc-end (context-arc c)))

  (define (context-now c)
    (lif (e (context-event c))
         (null? e)
         (context-start c)
         (event-beat e)))

  (define (context-serialised c)
    (list 'context
          (list 'arc (context-start c) (context-end c))
          (cons 'events (event-clean (context-events-next c)))))

  ;; For use as a record-writer in chez (see init.scm)
  (define (context-print c port wr)
    (put-datum port (context-serialised c)))

  (define context-with-events
    (case-lambda
      ((c nxt) (context-with-events c nxt '()))
      ((c nxt prv) (make-context nxt prv (context-arc c) (context-chain c)))))

  (define (context-clear-events c)
    (context-with-events c '()))

  (define (context-with-arc c new-arc)
    (make-context (context-events-next c)
                  (context-events-prev c)
                  new-arc
                  (context-chain c)))
  (define rearc context-with-arc) ; alias

  (define (context-length c)
    (arc-length (context-arc c)))

  (define (context-with-chain c chain)
    (make-context (context-events-next c)
                  (context-events-prev c)
                  (context-arc c)
                  chain))

  (define (context-append-chain c chain)
    (context-with-chain c (append chain (context-chain c))))

  ;; Pop the top lambda off the chain.
  (define (context-pop-chain c)
    (context-with-chain c (lif [ch (context-chain c)] (null? ch) '() (cdr ch))))

  ;; Pop the top lambda off the context's chain, and call it
  ;; with the context itself. This will be done recursively
  ;; up the chain.
  (define (context-resolve c)
    (lif [ch (context-chain c)] (null? ch) c ((car ch) (context-pop-chain c))))

  ;;--------------------------------------------------------------------
  ;; Iteration. A context has a list of previous and next events - these
  ;; functions assist in moving around in the context, as well as things like
  ;; mapping, filtering etc.

  ;; Public iteration functions
  (define (context-rewind c)
    (cond
      ((context-empty? c) c)
      ((context-last? c)
       (context-with-events
        c (reverse (cons (context-event c)
                         (context-events-prev c)))))
      (else (context-move c -9999999)))) ;; Lazy

  (define (context-move c n)
    (context-it c (until-zero-or-end n)))

  (define (context-to-closest-event c time)
    (context-it c (to-closest time)))

  (define (context-to-event-after c time)
    (context-it c (to-before time)))

  ;;----------------------------------------------------------------------
  ;; Transformations.

  ;; Used in context-map and context-filter.
  ;; context, (context, result-list -> result-list) -> context
  (define (context-reduce context event-reducer)
    (let loop ([c context] [output '()])
      (if (context-it-end? c)
          (context-with-events c (reverse output))
          (loop (context-move1-fwd c)
                (event-reducer c output)))))

  ;; (context -> event), context -> context
  ;; To return lists of events, supply 'append' for reducer.
  (define context-map
    (case-lambda
      ((new-event-fn context)
       (context-map new-event-fn context cons))

      ((new-event-fn context reducer)
       (context-reduce
        context (lambda (c output)
                  (lif [ev (new-event-fn c)] (null? ev) output (reducer ev output)))))))

  ;; (context -> bool), context -> context
  (define (context-filter pred context)
    (context-reduce
     context (lambda (c output)
               (if (pred c) (cons (context-event c) output) output))))

  ;; Removes events from the context that don't fall within arc.
  (define (context-trim context)
    (define (pred c)
      (between (event-beat (context-event c))
               (context-start c)
               (context-end c)))
    (context-filter pred context))

  ;; Sorts the events in a context by time. In general events should
  ;; be kept sorted in this way.
  (define (context-sort context)
    (let* ([events (context-events-next (context-rewind context))]
           [before? (lambda (e1 e2) (< (event-beat e1) (event-beat e2)))]
           [sorted (list-sort before? events)])
      (context-with-events context sorted)))

  ;; Merge two contexts - the new context's events are kept sorted.
  (define (contexts-merge c1 c2)
    (let ([c1 (context-rewind c1)]
          [c2 (context-rewind c2)])
      (make-context (merge-sorted (context-events-next c1)
                                  (context-events-next c2)
                                  event-before?)
                    '()
                    (make-arc (min (context-start c1) (context-start c2))
                              (max (context-end c1) (context-end c2)))
                    (context-chain c2))))

  ;;---------------------------------------------------------------------
  ;; Helpers used in public iteration functions.

  ;; Swap the order of next/prev to move one item forward or back.
  (define (context-move1 c get-next get-prev)
    (let ([next (get-next c)] [prev (get-prev c)])
      (if (null? next) c
          (context-with-events c (cdr next) (cons (car next) prev)))))
  (define (context-move1-fwd c)
    (context-move1 c context-events-next context-events-prev))
  (define (context-move1-bck c)
    (context-move1 c context-events-prev context-events-next))

  ;; direction-fn returns +1 for move fwd, -1 for back, 0 for stop.
  (define (context-it c direction-fn)
    (let* ([dir (direction-fn c)]
           [mv (if (positive? dir) context-move1-fwd context-move1-bck)])
      (if (zero? dir) c (context-it (mv c) direction-fn))))

  ;; direction-fn that drives context iteration forward/back n times,
  ;; for positive/negative n, or until the next/prev list runs out.
  (define (until-zero-or-end n)
    (lambda (context)
      (let ([x n]
            [get (if (positive? n)
                     context-events-next
                     context-events-prev)])
        (cond
          ((or (zero? x) (null? (get context))) 0)
          ((positive? x) (set! n (sub1 n)) x)
          ((negative? x) (set! n (add1 n)) x)))))

  ;; direction-fn that drives context iteration to the event closest
  ;; to the requested time.
  (define (to-closest time)
    (lambda (c)
      (define (get-delt bounds-check? default get-event)
        (if (bounds-check? c) default (delta time (get-event c))))
      (let ([cur (get-delt context-empty? 0 context-event)]
            [prv (get-delt context-first? -inf.0 context-prev-unchecked)]
            [nxt (get-delt context-last?  +inf.0 context-next-unchecked)])
        (if (<= (abs cur) (abs nxt))
            (if (< (abs prv) (abs cur)) -1 0) +1))))

  ;; direction-fn that moves a context to the first event that's greater
  ;; than or equal to the requested time.
  (define (to-before time)
    (lambda (c)
      (define (get-delt bounds-check? default get-event)
        (if (bounds-check? c) default (delta time (get-event c))))
      (let ([cur (get-delt context-empty? 0 context-event)]
            [prv (get-delt context-first? -inf.0 context-prev-unchecked)]
            [nxt (get-delt context-last?  +inf.0 context-next-unchecked)])
        (cond
          ((and (> cur 0) (>= nxt 0) (< nxt cur)) +1)
          ((and (< cur 0) (<= prv 0) (> prv cur)) -1)
          (else 0)))))

  (define (context-first? c)
    (null? (context-events-prev c)))

  (define (context-last? c)
    (or (context-empty? c)
        (null? (cdr (context-events-next c)))))

  (define (context-it-end? c)
    (null? (context-events-next c)))

  (define (context-empty? c)
    (and (null? (context-events-next c))
         (null? (context-events-prev c))))

  (define (context-next-unchecked c) (cadr (context-events-next c)))
  (define (context-prev-unchecked c) (car (context-events-prev c)))
  (define (delta t event) (- t (event-get event time-key +inf.0)))

  ;; Inserts the new event in a sorted fashion into the context, leaving the
  ;; context pointing to the new event, not the original one.
  (define (context-insert c new-event)
    (if (context-empty? c)
        (context-with-events c (list new-event))
        (let* ([moved (context-to-event-after c (event-beat new-event))]
               [nxt (context-events-next moved)]
               [prv (context-events-prev moved)])
          (context-with-events c (cons new-event nxt) prv))))

  ) ; end module context
