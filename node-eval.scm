(library (node-eval)
  (export
   splicer
   splicer?
   splicer-logic
   build-splicer
   get-leaf
   get-leaf-early
   render
   render-arc)
  
  (import (scheme) (utilities) (event) (context))

  ;;--------------------------------------------------------------
  ;; Call on the root of a tree to fill a context with events.
  (define (render item context)
    (context-trim (item context)))

  (define (render-arc p start end)
    (render p (make-context (make-arc start end))))

  ;; Evaluate a leaf. It might be a plain value, a naked context
  ;; procedure or an arc-moving-branch.
  (define (get-leaf leaf context)
    (cond
     ((procedure? leaf) (leaf context))
     (else leaf)))

  ;; If we must eval a leaf before adding a new event, the context will look wrong.
  ;; The event doesn't yet exist, so e.g. next/prev and seeding would be broken.
  ;; In this case, add an empty event to the context before evaluating.
  (define (get-leaf-early leaf time-to-add context)
    (if (procedure? leaf)
	(get-leaf leaf (context-insert context (make-event time-to-add)))
	leaf))

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

  )
