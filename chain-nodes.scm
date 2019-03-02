(library (chain-nodes)
  (export o-> x-> <-> mute print-nodes)
  
  (import (chezscheme) (utilities) (context) (event) (node-eval))

  ;; Useful for understanding how a context changes through a chain.
  (define print-nodes #f)

  ;;--------------------------------------------------------------
  ;; Apply all the branches to the input context.
  (define (x-> . nodes)
    (make-arc-mover
     (fold-nodes nodes)
     (fold-moves nodes)))

  ;; Apply the branches to a blank context, then merge with input.
  (define (o-> . nodes)
    (make-arc-mover
     (lambda (context)
       (let* ([blank (context-clear-events context)]
	      [filled ((fold-nodes nodes) blank)]
	      [arc (context-arc filled)])
	 (context-with-arc (contexts-merge context filled) arc)))
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

  ;; Branches wrapped in this will be skipped
  (define (mute . branches)
    (lambda (context) context))

  ;;---------------------------------------------------------------
  ;; To allow branches to look at the future/past of their source,
  ;; trunks return a struct containing the ordinary context-taking
  ;; lambda (as in any other branch) and another function, which
  ;; returns an arc describing how the contexts' arcs should be moved
  ;; from that point upward in the chain.
  (define (do-branch context b)
    (let ([c ((if (arc-mover? b) (arc-mover-logic b) b) context)])
      (print-node c)
      c))

  (define (do-arc-edit b arc)
    (if (arc-mover? b) ((arc-mover-move b) arc) arc))

  (define (fold-nodes fns)
    (lambda (context)
      (fold-left do-branch context fns)))
  
  (define (fold-moves fns)
    (lambda (arc)
      (fold-right do-arc-edit arc fns)))

  (define (print-node context)
    (when print-nodes
      (println "--------------------------")
      (println context)))

  (define (print-arc-move edit)
    (when print-nodes
      (println "--------------------------")
      (println (format "arc change: ~A, ~A"
		       (arc-start edit)
		       (arc-end edit)))
      (newline)))
  
  )
