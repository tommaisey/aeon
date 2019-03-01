(library (trunk)
  (export o-> x-> <-> render render-arc print-branches)
  
  (import (chezscheme) (utilities) (context) (event))

  ;; Useful for understanding how a context changes through a chain.
  (define print-branches #f)

  ;;--------------------------------------------------------------
  ;; Apply all the branches to the input context.
  (define (x-> . branch-fns)
    (make-arc-branch
     (fold-branches branch-fns)
     (fold-arc-edits branch-fns)))

  ;; Apply the branches to a blank context, then merge with input.
  (define (o-> . branch-fns)
    (make-arc-branch
     (lambda (context)
       (let* ([blank (context-clear-events context)]
	      [filled ((fold-branches branch-fns) blank)]
	      [arc (context-arc filled)])
	 (context-with-arc (contexts-merge context filled) arc)))
     (fold-arc-edits branch-fns)))

  ;; Same as x->, but also changes the arc 
  (define (<-> move-start move-end . branch-fns)
    (let ([edit (make-arc move-start move-end)])
      (make-arc-branch
       (lambda (context)
	 (let* ([filled ((fold-branches branch-fns) context)]
		[restored (context-add-arc filled (arc-negate edit))])
	   (print-arc-change edit)
	   restored))
       (lambda (arc)
	 (arc-add ((fold-arc-edits branch-fns) arc) edit)))))

  (define (make-trunk t branches)
    (arc-branch-code (apply t branches)))

  ;;--------------------------------------------------------------
  ;; Call on the root of a tree to fill a context with events. 
  (define (render p context)
    (if (arc-branch? p)
	(let ([code (arc-branch-code p)]
	      [top-arc ((arc-branch-edit p) (context-arc context))])
	  (code (context-with-arc context top-arc)))
	(p context)))

  (define (render-arc p arc)
    (render p (make-context arc)))

  ;;---------------------------------------------------------------
  ;; To allow branches to look at the future/past of their source,
  ;; trunks return a struct containing the ordinary context-taking
  ;; lambda (as in any other branch) and another function, which
  ;; returns an arc describing how the contexts' arcs should be moved
  ;; from that point upward in the chain.
  (define-record-type arc-branch
    (fields (immutable code)
	    (immutable edit)))

  (define (do-branch ctxt b)
    (let ([ctxt ((if (arc-branch? b) (arc-branch-code b) b) ctxt)])
      (print-branch ctxt)
      ctxt))

  (define (do-arc-edit b arc)
    (if (arc-branch? b) ((arc-branch-edit b) arc) arc))

  (define (fold-branches fns)
    (lambda (context)
      (fold-left do-branch context fns)))
  
  (define (fold-arc-edits fns)
    (lambda (arc)
      (fold-right do-arc-edit arc fns)))

  (define (print-branch context)
    (when print-branches
      (println "--------------------------")
      (println context)))

  (define (print-arc-change edit)
    (when print-branches
      (println "--------------------------")
      (println (format "arc change: ~A, ~A"
		       (arc-start edit)
		       (arc-end edit)))
      (newline)))
  
  )
