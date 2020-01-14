#!chezscheme ;; Needed for the extra symbols like +->

(library (nodes-chains)
  (export chain-docs o-> x-> +-> thru copies off)

  (import (chezscheme) 
          (utilities) (context) (event)
          (node-eval) (doc))

  (define (x-> . nodes)
    (let ([nodes (reverse nodes)])
      (lambda (context)
        (context-resolve (context-push-chain context nodes)))))

  (define (o-> . nodes)
    (let ([nodes (reverse nodes)])
      (lambda (context)
        (let ([inner (context-with-chain context nodes)])
          (contexts-merge (context-resolve inner)
                          (context-resolve context))))))

  (define (+-> . nodes)
    (lambda (context)
      (let ([context (make-caching-context context)])
        (define (sum c node)
          (contexts-merge (node context) c))
        (fold-left sum context nodes))))

  ;;--------------------------------------------------------------
  (define (off . args)
    (lambda (context) (context-resolve context)))

  (alias thru x->)
  (alias copies +->)

  ;;--------------------------------------------------------------
  (make-doc chain-docs
    (x-> "Applies any number of operators successively to an input. 
Replaces the input."
         ((nodes [/... Operator] "0 or more pattern operator nodes"))
         ())

    (o-> "Applies any number of operators successively, starting with 
an empty events list. Merges the result with the input."
         ((nodes [/... Operator] "0 or more pattern operator nodes"))
         ())

    (+-> "Applies each operator to the input separately, summing the 
results to a blank context. Replaces the input."
         ((nodes [/... Operator] "0 or more pattern operator nodes"))
         ())

    (off "A quick way to disable an operator and its sub-operators.
All arguments are ignored, and a no-op is returned."
         ((args [/... Any] "0 or more or anything, to be ignored"))
         ()))

  )
