;; Some functions to evaluate the graph, or chunks of it.
;; These can't live in context.scm, because we'd create a
;; circular dependency between that file and seq-eval.
(library (context-render)
  (export
    render-arc
    context-resolve
    make-caching-context)

  (import (scheme)
          (utilities)
          (arc)
          (context)
          (seq-eval)
          (matchable))

  ;;--------------------------------------------------------------
  ;; Call on the root of a tree to fill a context with events.
  (define render-arc
    (case-lambda
      ((pattern-fn arc)
       (context-trim (pattern-fn (make-context arc))))

      ((pattern-fn start end)
       (render-arc pattern-fn (make-arc start end)))))

  ;; Pop the top lambda off the context's chain, and call it
  ;; with the context itself. This will be done recursively
  ;; up the chain.
  (define (context-resolve c)
    (lif (ch (context-chain c)) (null? ch) c
         (eval-seq (car ch) (context-pop-chain c))))

  ;; Makes a context with a special caching function added behind the
  ;; top fn in the chain. Repeated requests for the same arc return a
  ;; cached result.
  (define (make-caching-context context)
    (define (test-arc arc-fn c1 c2)
      (arc-fn (context-arc c1) (context-arc c2)))
    (define (make-cacher)
      (let ([cached-list '()])
        (lambda (ctxt)
          (let loop ([cl cached-list])
            (match cl
              [() (begin
                    (set! cached-list (cons (context-resolve ctxt) cached-list))
                    (car cached-list))]
              [(c . rst) (cond [(test-arc arc-eq? c ctxt) c]
                               [(test-arc arc-contains? c ctxt)
                                (context-trim (rearc c (context-arc ctxt)))]
                               [else (loop rst)])])))))

    (lif (chain (context-chain context)) (null? chain) context
         (context-push-chain (context-pop-chain context)
                             (list (car chain) (make-cacher))))))
