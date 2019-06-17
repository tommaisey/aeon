(library (time-nodes)
  (export mv- mv+ mv/ mv*
          flip-time swing taps)

  (import (chezscheme) (utilities) (context) (event)
          (chunking)
          (node-eval)
          (chain-nodes)
          (rhythm))

  ;; A general 'mv', taking a math op and a def. The math op is
  ;; called with each segment's current time and the value returned by
  ;; def. The input context is resolved with a different arc, in effect
  ;; shifting it in time.
  (define (mv math-op inv-math-op . pdefs)
    (apply x-> (pdefs-to-nodes pdefs (mv-math-impl math-op inv-math-op))))

  (define (mv+ . pdefs) (apply mv + - pdefs))
  (define (mv- . pdefs) (apply mv - + pdefs))
  (define (mv* . pdefs) (apply mv * / pdefs))
  (define (mv/ . pdefs) (apply mv / * pdefs))

  ;;-------------------------------------------------------------------
  ;; Swing - implemented as a sine wave, so that notes off the main beat
  ;; are moved progressively more as they get closer to it.
  (define (swing period amount)

    (define (mover context)
      (let* ([period (get-leaf period context)]
             [amount (get-leaf amount context)]
             [now (context-now context)]
             [ev (context-event context)]
             [b (event-beat ev)]
             [offset (+ b (* 3 (/ period 2)))]
             [t (+ b (range-sine (* 2 period) 0 (* amount period) offset))]
             [sus (event-get ev ':sustain #f)])
        (if sus
            (event-set-multi ev (:beat t) (':sustain (- sus (- t b))))
            (event-set-multi ev (:beat t)))))

    (unless (between amount 0.0 1.001) 
      (error 'swing "amount should be in the range 0 <-> 1" amount))

    (lambda (context)
      (let* ([s (context-start context)]
             [e (context-end context)]
             [c (context-resolve (rearc context (make-arc (- s period) e)))])
        (context-trim (rearc (context-map mover c) (make-arc s e))))))

  ;;-------------------------------------------------------------------
  ;; Taps is like a MIDI delay effect, but it can operate in reverse.
  ;; The optional node arguments are applied to the taps differently.
  ;; `iterative-node` is applied once to the 1st tap, twice to the 2nd, etc.
  ;; `once-node` is applied once to all taps but not the original.
  (define taps
    (case-lambda
      ((period num) (taps period num (off)))
      ((period num iterative-node) (taps period num iterative-node (off)))
      ((period num iterative-node once-node)

       ;; Builds a list of pairs of times and their indeces. 
       ;; Omits the original 'src' time.
       (define (list-taps src period num start end)
         (if (zero? num) (list)
             (let* ([sign (if (> num 0) 1 -1)]
                    [time-flt (lambda (t-i) (between (car t-i) start end))]
                    [time-idx (lambda (i) (cons (+ src (* (inc i) sign period)) (inc i)))])
               (filter time-flt (map time-idx (iota (abs (- num sign))))))))

       ;; Sets the new time and repeatedly applies iterative-node to an event.
       ;; Has to wrap then unwrap the event in a context so iterative-node
       ;; can work on it individually.
       (define (tap-maker ev period)
         (lambda (time-and-idx)
           (let* ([t (car time-and-idx)]
                  [i (cdr time-and-idx)]
                  [c (make-context (list (event-set ev :beat t))
                                   (make-arc t (+ t period)))])
             (context-event ((apply x-> (repeat iterative-node i)) c)))))

       (check-type integer? num 'taps)

       (lambda (context)
         (let* ([orig-arc (context-arc context)]
                [s (arc-start orig-arc)]
                [e (arc-end orig-arc)]
                [p (get-leaf period context)]
                [n (get-leaf num context)]
                [len (* p n)]
                [a (if (> len 0)
                       (arc-with-start orig-arc (- s len))
                       (arc-with-end orig-arc (- e len)))]
                [c (context-resolve (rearc context a))]
                [events (context-events-next c)])

           (define (build-taps result ev)
             (let ([times-and-indeces (list-taps (event-beat ev) p n s e)])
               (append (map (tap-maker ev p) times-and-indeces) result)))

           (contexts-merge
            (rearc c orig-arc)
            (once-node (make-context (fold-left build-taps '() events) orig-arc))))))))

  ;;-------------------------------------------------------------------
  ;; Flips time of events within each chunk.
  ;; e.g. with chunk = 2:
  ;; 0.1 -> 1.9 and vice versa.
  ;; 2.1 -> 3.9 and vice versa
  ;; 1 -> 1
  ;; TODO: I think this requests more from its source than needed, but
  ;; it's hard to get it right at chunk boundaries. Investigate.
  (define (flip-time chunk)

    (define (flip-chunked chunk x)
      (let* ([x-trunc (snap-prev x chunk)]
             [x-mod (- x x-trunc)])
        (+ x-trunc (abs (- x-mod chunk)))))

    (define (move-event start end)
      (lambda (context)
        (let* ([e (context-event context)]
               [t (flip-chunked chunk (event-beat e))])
          (if (between t start end)
              (event-set e time-key t)
              (list))))) ;; empty events are ignored by context-map

    (lambda (context)
      (let* ([s (- (context-start context) chunk)]
             [e (snap-next (context-end context) chunk)]
             [c (context-resolve (rearc context (make-arc s e)))]
             [c (context-sort (context-map (move-event s e) c))])
        (context-trim (context-with-arc c (context-arc context))))))
  
  ;;-------------------------------------------------------------------
  ;; Helper for building nodes from a list of pdefs
  ;; (pdef ...), impl -> ((context -> context) ...)
  (define (pdefs-to-nodes pdefs impl)
    (map (lambda (p) (lambda (c) (dispatch-pdef p c impl))) pdefs))

  ;;-------------------------------------------------------------------
  ;; Implementation functions to be supplied to a chunking algorithm.

  ;; Resolves input context with an arc shifted by the leaf value,
  ;; effectively moving a different slice of time into this one.
  (define (mv-math-impl math-fn inv-math-fn)
    (define (mapper new-val)
      (lambda (context)
        (event-move (context-event context) new-val math-fn)))
    (lambda (context leaf)
      (derecord context ([old-start context-start]
                         [old-end context-end])
        (context-events-next
         (let ([val (get-leaf-early leaf old-start context)])
           (cond
             ((is-rest? val) (context-resolve context))
             ((not (number? val)) (raise (pattern-error "'mv'" "number" val)))
             (else
               (let* ([new-start (inv-math-fn old-start val)]
                      [new-end (inv-math-fn old-end val)]
                      [shifted (rearc context (make-arc new-start new-end))])
                 (context-map (mapper val) (context-resolve shifted))))))))))


  )
