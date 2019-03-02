(library (node-eval)
  (export
   arc-mover
   arc-mover?
   arc-mover-logic
   arc-mover-move
   make-arc-mover
   get-leaf
   get-leaf-early
   render
   render-arc)
  (import (scheme) (event) (context))

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

  (define-record-type arc-mover
    (fields (immutable logic)
	    (immutable move)))

  )
