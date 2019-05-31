(library (time-nodes)
  (export mv- mv+ mv/ mv*
	  flip-time)

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
