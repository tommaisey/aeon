;; -*- geiser-scheme-implementation: chez-*-

(library (pattern-node)
  (export ->>)
  (import (chezscheme) (utilities))

  ;; A very simple unary threading macro.
  ;;
  ;; (->> my-pipeline
  ;;     (lambda (x) (+ x 1))
  ;;     (lambda (x) (* x 2))
  ;;     display)
  ;;
  ;; (my-thread 3) ; prints 8
  (define-syntax ->>
    (syntax-rules ()

      ((->> transformers ...)
       (lambda (context)
	 (fold-left (lambda (c t) (t c)) context (list transformers ...))))
      
      ((->> name transformers ...)
       (define name (->> transformers ...)))))

  ) ; end library pattern-node
