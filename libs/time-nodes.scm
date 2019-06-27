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
    (let ([impl (mv-math-impl math-op inv-math-op)])
      (apply x-> (map (lambda (p) (wrap-transform-fn impl p)) pdefs))))

  (define (mv+ . pdefs) (apply mv + - pdefs))
  (define (mv- . pdefs) (apply mv - + pdefs))
  (define (mv* . pdefs) (apply mv * / pdefs))
  (define (mv/ . pdefs) (apply mv / * pdefs))

  ;;-------------------------------------------------------------------
  ;; Swing - implemented as a sine wave, so that notes off the main beat
  ;; are moved progressively more as they get closer to it.
  (define (swing period amount)

    (define (mover context)
      (let* ([period (eval-leaf period context)]
             [amount (eval-leaf amount context)]
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
      ((period num) (taps period num nowt))
      ((period num iterative-node) (taps period num iterative-node nowt))
      ((period num iterative-node once-node)

       ;; Compute furthest lookahead/lookback that might be required.
       (define possible-range
         (let ([min-p (maybe-leaf-meta period leaf-range-min)]
               [max-p (maybe-leaf-meta period leaf-range-max)]
               [min-n (maybe-leaf-meta num leaf-range-min)]
               [max-n (maybe-leaf-meta num leaf-range-max)])
           (if (for-all identity (list min-p max-p min-n max-n))
               (let ([values (list (* min-p min-n)
                                   (* min-p max-n)
                                   (* max-p min-n)
                                   (* max-p max-n))])
                 (list (apply min values) (apply max values)))
               (error 'taps "period and num must specify definite ranges" 
                      (list period num)))))

       ;; Builds a list of pairs of times and their indeces.
       ;; Omits the original 'src' time.
       (define (list-taps src period num start end)
         (if (zero? num) (list)
             (let* ([sign (if (> num 0) 1 -1)]
                    [time-flt (lambda (t-i) (between (car t-i) start end))]
                    [time-idx (lambda (i) (cons (+ src (* (inc i) sign period)) 
                                                (inc i)))])
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

       ;; Builds a list of events (the taps) based on the context's current event.
       (define (build-taps start end)
         (lambda (context)
           (let* ([t (context-now context)]
                  [period (eval-leaf period context)]
                  [num (eval-leaf num context)]
                  [times-and-indeces (list-taps t period num start end)])
             (map (tap-maker (context-event context) period) times-and-indeces))))

       ;; Just catches a common error (reversing period and num).
       (if (and (number? num) (not (integer? num)))
           (error 'taps "number of taps should be an integer" num))

       (lambda (context)
         (let* ([orig-arc (context-arc context)]
                [s (arc-start orig-arc)]
                [e (arc-end orig-arc)]
                [a (make-arc (- s (apply max 0 possible-range))
                             (- e (apply min 0 possible-range)))]
                [c (context-resolve (rearc context a))]
                [c-taps (context-map (build-taps s e) c append)])

           (contexts-merge
            (rearc c orig-arc)
            (once-node (rearc c-taps orig-arc))))))))

  ;;-------------------------------------------------------------------
  ;; Flips time of events within each chunk.
  ;; e.g. with chunk = 2:
  ;; 0.1 -> 1.9 and vice versa.
  ;; 2.1 -> 3.9 and vice versa
  ;; 1 -> 1
  ;; TODO: I think this requests more from its source than needed, but
  ;; it's hard to get it right at chunk boundaries. Investigate.
  (define (flip-time chunk)

    (define (flip-chunked x)
      (let* ([x-trunc (snap-prev x chunk)]
             [x-mod (- x x-trunc)])
        (+ x-trunc (abs (- x-mod chunk)))))

    (define (move-event start end)
      (lambda (context)
        (let* ([e (context-event context)]
               [t (flip-chunked (event-beat e))])
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
  ;; Implementation functions to be supplied to a chunking algorithm.

  ;; Resolves input context with an arc shifted by the leaf value,
  ;; effectively moving a different slice of time into this one.
  (define (mv-math-impl math-fn inv-math-fn)
    (define (mapper new-val)
      (lambda (context)
        (event-move (context-event context) new-val math-fn)))
    (lambda (context leaf)
      (let* ([old-arc (context-arc context)]
             [val (eval-leaf-early leaf (arc-start old-arc) context)])
        (cond
          ((is-rest? val) (context-resolve context))
          ((not (number? val)) (error 'mv "number" val))
          (else
            (let* ([shifted (rearc context (arc-math old-arc inv-math-fn val))]
                   [shifted (context-resolve shifted)])
              (context-trim (rearc (context-map (mapper val) shifted) old-arc))))))))

  )
