#!chezscheme ;; Needed for the extra symbols like »

(library (pdef)
  (export runtime-pdef pdef • » × !)
  (import (scheme) (utilities) (node-eval))

  ;;------------------------------------------------------------------
  ;; A special marker used by these macros
  (define !) ;; Denotes a list that shouldn't be evaluated

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
	def))

  ;;-------------------------------------------------------------------
  ;; The old macro pdef system. Currently exploring a more runtime/eval
  ;; based solution, which can more accurately escape symbols in our
  ;; notation through patterndef.
  (define-syntax pdef
    (lambda (x)
      (syntax-case x (• » × !)

	((m (× v n))
	 (syntax (build-splicer '× v n)))

	((m (» v n))
	 (syntax (build-splicer '» v n)))

	((m (• v ...))
	 (syntax (pdef ('• v ...))))

	((m (! v ...)) ;; ! escapes, don't evaluate
	 (syntax (list (pdef v) ...)))
	
	((m (v q ...)) (identifier? (syntax v))
	 (syntax (v q ...))) ;; evaluate as function
	
	((m (v ...))
	 (syntax (pdef (! v ...))))

	((m v)
	 (syntax v)))))

  )
