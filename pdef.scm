#!chezscheme ;; Needed for the extra symbols like »

(library (pdef)
  (export pdef runtime-pdef)
  (import (scheme) (utilities) (node-eval))

  (define-syntax pdef
    (syntax-rules ()
      ((_ lst)
       (runtime-pdef 'lst))))
  
  ;; Excluding all the musical symbols we want to use is hard
  ;; at compile time. This is a sketch of a function that evaluates
  ;; a pdef at runtime, checking symbols in the first position and
  ;; only evaluating as functions those which return false for
  ;; special-pdef-value? - currently a test stub.
  ;;
  ;; This is rather naughty because it uses eval a lot, which is
  ;; considered bad practice. But how am I going to check for
  ;; runtime special symbols otherwise?
  (define (runtime-pdef def)
    (define env (interaction-environment))
    (define sym-is-fn? (if (top-level-bound? 'pdef-fn?)
			   (top-level-value 'pdef-fn?)
			   (lambda (sym) #t)))
    (if (unsafe-list? def)
	(if (and (symbol? (car def))
		 (sym-is-fn? (car def)))
	    (eval def env)
	    (let loop ([p def] [x '()])
	      (if (null? p)
		  (reverse x)
		  (loop (cdr p) (if (unsafe-list? (car p))
				    (cons (runtime-pdef (car p)) x)
				    (cons (eval (car p) env) x))))))
	(eval def env)))
  )
