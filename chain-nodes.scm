#!chezscheme ;; Needed for the extra symbols like +->

(library (chain-nodes)
  (export o-> x-> +-> mute)
  
  (import (chezscheme) (utilities) (context) (event) (node-eval))

  ;;--------------------------------------------------------------
  ;; Apply each node successively to the input context.
  ;; Replace the input.
  (define (x-> . nodes)
    (lambda (context)
      (context-resolve (context-append-chain context (reverse nodes)))))

  ;; Apply each node successively to a blank context.
  ;; Merge result with the input.
  (define (o-> . nodes)
    (lambda (context)
      (let ([inner (context-with-chain context (reverse nodes))])
	(contexts-merge (context-resolve inner)
			(context-resolve context)))))

  ;; Apply each node to the input separately, summing the results
  ;; to a blank context. Replace the input.
  (define (+-> . nodes)
    (lambda (context)
      (define (sum c node)
	(contexts-merge (node context) c))
      (fold-left sum context nodes)))

  ;; Nodes wrapped in this will be skipped
  (define (mute . nodes)
    (lambda (context) (context-resolve context)))
  
  )
