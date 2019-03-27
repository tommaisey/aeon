#!chezscheme ;; Needed for the extra symbols like +->

(library (chain-nodes)
  (export o-> x-> <-> +-> mute print-nodes)
  
  (import (chezscheme) (utilities) (context) (event) (node-eval))

  ;; Useful for understanding how a context changes through a chain.
  (define print-nodes #f)

  ;;--------------------------------------------------------------
  ;; Apply each node successively to the input context.
  ;; Replace the input.
  (define (x-> . nodes)
    (make-arc-mover
     (fold-nodes nodes)
     (fold-moves nodes)))

  ;; Apply each node successively to a blank context.
  ;; Merge result with the input.
  (define (o-> . nodes)
    (make-arc-mover
     (lambda (context)
       (let* ([blank (context-clear-events context)]
	      [filled ((fold-nodes nodes) blank)]
	      [arc (context-arc filled)])
	 (context-with-arc (contexts-merge context filled) arc)))
     (fold-moves nodes)))

  ;; Apply each node to the input separately, summing the results
  ;; to a blank context. Replace the input.
  (define (+-> . nodes)
    (make-arc-mover
     (lambda (context)
       (let ([summed ((sum-nodes nodes) context)]
	     [arc (context-arc context)])
	 (context-with-arc summed arc)))
     (fold-moves nodes)))

  ;; Same as x->, but also changes the arc 
  (define (<-> move-start move-end . nodes)
    (let ([edit (make-arc move-start move-end)])
      (make-arc-mover
       (lambda (context)
	 (let* ([filled ((fold-nodes nodes) context)]
		[restored (context-add-arc filled (arc-negate edit))])
	   (print-arc-move edit)
	   restored))
       (lambda (arc)
	 (arc-add ((fold-moves nodes) arc) edit)))))

  ;; Nodes wrapped in this will be skipped
  (define (mute . nodes)
    (lambda (context) context))

  ;;---------------------------------------------------------------
  ;; To allow nodes to look at the future/past of their source,
  ;; trunks return a struct containing the ordinary context-taking
  ;; lambda (as in any other node) and another function, which
  ;; returns an arc describing how the contexts' arcs should be moved
  ;; from that point upward in the chain.
  (define (do-node context b)
    (let ([c ((if (arc-mover? b) (arc-mover-logic b) b) context)])
      (print-node-context c)))

  (define (sum-from original)
    (lambda (context b)
      (contexts-merge (do-node original b) context)))

  (define (do-arc-edit b arc)
    (if (arc-mover? b) ((arc-mover-move b) arc) arc))

  (define (fold-nodes fns)
    (lambda (context)
      (fold-left do-node context fns)))

  (define (sum-nodes fns)
    (lambda (context)
      (let ([blank (context-clear-events context)])
	(fold-left (sum-from context) blank fns))))
  
  (define (fold-moves fns)
    (lambda (arc)
      (fold-right do-arc-edit arc fns)))

  (define (print-node-context context)
    (when print-nodes
      (println "--------------------------")
      (println context))
    context)

  (define (print-arc-move edit)
    (when print-nodes
      (println "--------------------------")
      (println (format "arc change: ~A, ~A"
		       (arc-start edit)
		       (arc-end edit)))
      (newline)))
  
  )
