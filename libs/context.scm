;; -*- geiser-scheme-implementation: chez-*-
(library (context)
  (export
    context?
    make-context
    make-empty-context
    context-event
    context-events-next
    context-events-prev
    context-arc
    context-chain
    context-start
    context-end
    context-now
    context-insert
    context-with-events
    context-replace-event
    context-with-arc rearc
    context-with-chain
    context-pop-chain
    context-push-chain
    context-subdivide-fn
    context-with-subdivide-fn
    context-no-subdivide-fn
    context-length
    context-move
    context-to-event-after
    context-rewind
    context-map
    context-filter
    context-trim
    context-sort
    context-empty?
    context-first?
    context-last?
    context-it-end?
    contexts-merge)
  (import (chezscheme) (utilities) (arc) (event))

  ;; ---------------------------------------------
  ;; A context represents an arc of time, the events inside that arc, and
  ;; also a 'cursor' pointing to one of those events.
  ;;
  ;; The events, if resolved, are stored in two lists:
  ;; - events-next contains the current 'cursor' event and the following events.
  ;; - events-prev contains the previous events in reverse order.
  ;; Iteration consists of moving the current event into the 'prev' list.
  ;;
  ;; However, the context might be as-yet 'unresolved', meaning that it
  ;; consists only of a list of functions which can materialise the events
  ;; on request (see context-resolve in node-eval). This is the 'chain'.
  ;;
  ;; Calling the first fn in the chain should materialise the events - it does
  ;; so by calling the second one, etc. This allows each of the fns to call
  ;; their ancestors with different time chunks, to acheive lookahead.
  ;;
  ;; The 'subdivide-fn' allows a useful inversion-of-control. Often
  ;; we want to give a context to some unknown time-slicing algorithm
  ;; and have it call a callback for each chunk of the input context.
  ;; For example, `in:`, `to:` etc. place such a callback in subdivide-fn,
  ;; and call `over` or somesuch which controls where they add/change events.
  (define-immutable-record context
    [arc (make-arc 0 1)]
    [events-next '()]
    [events-prev '()]
    [chain '()]
    [subdivide-fn #f])

  (define (make-empty-context start end)
    (make-context (make-arc start end)))

  (define* (context-with-events c nxt [/opt (prv '())])
    (context-with-events-prev (context-with-events-next c nxt) prv))

  (alias rearc context-with-arc)

  (define (context-event c)
    (lif (n (context-events-next c)) (null? n) '() (car n)))

  (define (context-start c) (arc-start (context-arc c)))
  (define (context-end c) (arc-end (context-arc c)))

  (define (context-now c)
    (lif (e (context-event c))
         (null? e)
         (context-start c)
         (event-beat e)))

  ;; Replaces the pointed-to event, or simply adds if there is none.
  (define (context-replace-event c new-event)
    (if (context-it-end? c)
        (context-insert c new-event)
        (context-with-events c (cons new-event (cdr (context-events-next c)))
                               (context-events-prev c))))

  (define (context-length c)
    (arc-length (context-arc c)))

  ;; Pop the top lambda off the chain.
  (define (context-pop-chain c)
    (context-with-chain c (lif [ch (context-chain c)] (null? ch) '() (cdr ch))))

  ;; Push a lambda or list of lambdas onto the chain.
  (define (context-push-chain c node)
    (context-with-chain c (push-front node (context-chain c))))

  (define (context-no-subdivide-fn c)
    (context-with-subdivide-fn c #f))

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

  (define (context-to-event-after c time)
    (context-it c (to-after time)))

  ;;----------------------------------------------------------------------
  ;; Transformations.

  ;; Used in context-map and context-filter.
  ;; The reducer takes a context pointing at a particular event
  ;; and the current list of result events, and returns a new list
  ;; of result events.
  (define (context-reduce context event-reducer-fn)
    (let loop ([c context] [output '()])
      (if (context-it-end? c)
          (context-with-events c (reverse output))
          (loop (context-fwd1 c)
                (event-reducer-fn c output)))))

  ;; new-event-fn is a function taking a context (pointing at a particular
  ;; event) and returning a new event or nothing.
  ;; If you want new-event-fn to return lists of events,
  ;; supply 'append' for the argument reducer.
  (define* (context-map new-event-fn context [/opt (reducer cons)])
    (define (rdc c events-list)
      (lif [ev (new-event-fn c)]
           (null? ev) events-list (reducer ev events-list)))
    (context-reduce context rdc))

  ;; (context -> bool), context -> context
  (define (context-filter pred context)
    (define (rdc c events-list)
      (if (pred c) (cons (context-event c) events-list) events-list))
    (context-reduce context rdc))

  ;; Removes events from the context that don't fall within arc.
  (define (context-trim context)
    (define (pred c)
      (between? (event-beat (context-event c))
                (context-start c)
                (context-end c)))
    (context-filter pred context))

  ;; Sorts the events in a context by time. In general events should
  ;; be kept sorted in this way.
  (define (context-sort context)
    (let* ([events (context-events-next (context-rewind context))]
           [sorted (list-sort event-before? events)])
      (context-with-events context sorted)))

  ;; Merge two contexts - the new context's events are kept sorted.
  (define (contexts-merge c1 c2)
    (let ([c1 (context-rewind c1)]
          [c2 (context-rewind c2)])
      (make-context (make-arc (min (context-start c1) (context-start c2))
                              (max (context-end c1) (context-end c2)))
                    (merge-sorted event-before?
                                  (context-events-next c1)
                                  (context-events-next c2))
                    (list)
                    (context-chain c2)
                    (context-subdivide-fn c2))))

  ;;---------------------------------------------------------------------
  ;; Helpers used in public iteration functions.

  ;; Swap the order of next/prev to move one item forward or back.
  (define (context-move1 c get-next get-prev)
    (let ([next (get-next c)] [prev (get-prev c)])
      (if (null? next) c
          (context-with-events c (cdr next) (cons (car next) prev)))))

  (define (context-fwd1 c)
    (context-move1 c context-events-next context-events-prev))
  (define (context-bck1 c)
    (context-move1 c context-events-prev context-events-next))

  ;; direction-fn returns +1 for move fwd, -1 for back, 0 for stop.
  (define (context-it c direction-fn)
    (let* ([dir (direction-fn c)]
           [mv (if (positive? dir) context-fwd1 context-bck1)])
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

  ;; Returns delta of current event, next event and prev event.
  ;; If no next: +inf, if no prev: -inf.
  (define (get-deltas c time)
    (let ([delta (lambda (event) (- time (event-beat event)))])
      (values
       (if (context-empty? c) #f (delta (context-event c)))
       (if (context-last?  c) #f (delta (context-next-unchecked c)))
       (if (context-first? c) #f (delta (context-prev-unchecked c))))))

  ;; direction-fn that moves a context to the first event that's greater
  ;; than or equal to the requested time. Assumes context's events are sorted
  ;; by time.
  (define (to-after time)
    (lambda (c)
      (let-values ([(cur nxt prv) (get-deltas c time)])
        (cond
          ((and cur nxt (> cur 0)) +1)
          ((and prv (< prv 0)) -1)
          (else 0)))))

  (define (context-first? c)
    (null? (context-events-prev c)))

  (define (context-last? c)
    (or (context-empty? c)
        (null? (cdr (context-events-next c)))))

  (define (context-it-end? c)
    (or (null? (context-events-next c))
        (>= (context-now c) (context-end c))))

  (define (context-empty? c)
    (and (null? (context-events-next c))
         (null? (context-events-prev c))))

  (define (context-next-unchecked c) (cadr (context-events-next c)))
  (define (context-prev-unchecked c) (car (context-events-prev c)))

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
