#!chezscheme ;; Needed for the extra symbols like »

(library (node-eval)
  (export
   arc-mover
   arc-mover?
   arc-mover-logic
   arc-mover-move
   make-arc-mover
   splicer
   splicer?
   splicer-logic
   build-splicer
   get-leaf
   get-leaf-early
   render
   render-arc
   × » •
   repeat-sym
   sustain-sym
   rest-sym)
  (import (scheme) (utilities) (event) (context))

  ;;--------------------------------------------------------------
  ;; Call on the root of a tree to fill a context with events. 
  (define (render p context)
    (if (arc-mover? p)
	(let ([code (arc-mover-logic p)]
	      [top-arc ((arc-mover-move p) (context-arc context))])
	  (code (context-with-arc context top-arc)))
	(p context)))

  (define (render-arc p arc)
    (render p (make-context arc)))

  ;; Evaluate a leaf. It might be a plain value, a naked context
  ;; procedure or an arc-moving-branch.
  (define (get-leaf leaf context)
    (cond
	((procedure? leaf) (leaf context))
	((arc-mover? leaf) (render (arc-mover-logic leaf) context))
	(else leaf)))

  ;; If we must eval a leaf before adding a new event, the context will look wrong.
  ;; The event doesn't yet exist, so e.g. next/prev and seeding would be broken.
  ;; In this case, add an empty event to the context before evaluating.
  (define (get-leaf-early leaf time-to-add context)
    (let ([proc? (procedure? leaf)]
	  [arcb? (arc-mover? leaf)])
      (if (or proc? arcb?)
	  (let ([dummy-ctx (context-insert context (make-event time-to-add))])
	    (if proc?
		(leaf dummy-ctx)
		(render (arc-mover-logic leaf) dummy-ctx)))
	  leaf)))

  ;; A special type that denotes a context-function which wants to mutate
  ;; the context it receives as input to get a given output.
  (define-record-type arc-mover
    (fields (immutable logic)
	    (immutable move)))

  ;; A special type that denotes a function that can return a list
  ;; to be spliced into the containing pdef list. This must be evaluated
  ;; greedily while we work out the number of events in the pattern.
  ;; "type" can be either '& (repeating) or '_ (length).
  (define-record-type splicer
    (fields (immutable logic)))

  ;; Returns a splicer that repeats something or lengthens it.
  (define (build-splicer type val num)
    (make-splicer (lambda (context)
		    (cons (get-leaf val context)
			  (repeat (get-leaf type context)
				  (- (get-leaf num context) 1))))))
  
  (define × '×) ;; Denotes a repeated value in a pdef
  (define » '») ;; Denotes a sustained value in a pdef
  (define • '•) ;; Denotes a musical rest in a pdef
  (define repeat-sym ×)
  (define sustain-sym »)
  (define rest-sym •)

  )
