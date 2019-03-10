#!chezscheme ;; Needed for the extra symbols like »

(library (pdef)
  (export pdef • » × !)
  (import (scheme) (node-eval))

  ;;------------------------------------------------------------------
  ;; A special marker used by these macros
  (define !) ;; Denotes a list that shouldn't be evaluated

  ;;-------------------------------------------------------------------
  ;; Allows definition of nested lists without (quasi)quoting.
  ;;
  ;; Lists not starting with an identifier are just a list.
  ;; Lists starting with ! are just a list (prevents evaluation as a fn)
  ;; Lists starting with any other identifier are evaluated as functions. 
  ;;
  ;; (auto-quasi (1 "hi" (+ 2 2) (5 (+ 5 5)))) => (1 "hi" 4 (5 10))
  ;; (define x 7)
  ;; (auto-quasi (x 8 9)   => error, calls x as a fn
  ;; (auto-quasi (! x y z) => (7 8 9)
  ;; Some characters with special meaning in pdefs, namely • » ×
  ;; which denote a rest, a sustain and a repeat respectively.
  
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
