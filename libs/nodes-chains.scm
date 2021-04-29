(library (nodes-chains)

  (export chain-docs
          off part with copy)

  (import (chezscheme)
          (utilities)
          (context)
          (event)
          (node-eval)
          (doc))

  (define (off . args)
    (lambda (context) (context-resolve context)))

  (define (with . ops)
    (let ([ops (reverse ops)])
      (lambda (context)
        (context-resolve (context-push-chain context ops)))))

  (define (part . ops)
    (let ([ops (reverse ops)])
      (lambda (context)
        (let ([inner (context-with-chain context ops)])
          (contexts-merge (context-resolve inner)
                          (context-resolve context))))))

  (define (copy . ops)
    (lambda (context)
      (let ([context (make-caching-context context)])
        (define (sum c node)
          (contexts-merge (node context) c))
        (fold-left sum context ops))))

  ;;--------------------------------------------------------------
  (make-doc chain-docs
    (with "Applies any number of operators successively to an input.
Replaces the input."
          ((ops [/... Operator] "0 or more pattern operators"))
          ())

    (part "Applies any number of operators successively, starting with
an empty events list. Merges the result with the input."
          ((ops [/... Operator] "0 or more pattern operators"))
          ())

    (copy "Applies each operator to the input separately, summing the
results to a blank context. Replaces the input."
          ((ops [/... Operator] "0 or more pattern operators"))
          ())

    (off "A quick way to disable an operator and its sub-operators.
All arguments are ignored, and a no-op is returned."
         ((args [/... Any] "0 or more or anything, to be ignored"))
         ()))

  )
