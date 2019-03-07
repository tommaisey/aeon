(library (auto-quasi)
  (export auto-quasi pdef-quasi ~ !)
  (import (scheme) (node-eval))

  ;;------------------------------------------------------------------
  ;; Two special markers used by these macros
  (define ~) ;; Denotes a musical rest in a pdef
  (define !) ;; Denotes a list that shouldn't be evaluated
  
  ;;------------------------------------------------------------------
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
  
  (define-syntax auto-quasi
    (lambda (x)
      (syntax-case x (!)

	((_ (! v ...)) ;; ! escapes, don't evaluate
	 (syntax (list (auto-quasi v) ...)))
	
	((_ (v rest ...)) (identifier? (syntax v))
	 (syntax (v rest ...))) ;; evaluate as function
	
	((_ (v ...))
	 (syntax (auto-quasi (! v ...))))
     
	((_ v)
	 (syntax v)))))


  ;;-------------------------------------------------------------------
  ;; Same as auto-quasi, but adds characters with special meaning in pdefs.
  
  (define-syntax pdef-quasi
    (lambda (x)
      (syntax-case x (~ !)

	((_ (~ v ...))
	 (syntax (pdef-quasi ('~ v ...))))

	((_ (! v ...)) ;; ! escapes, don't evaluate
	 (syntax (list (pdef-quasi v) ...)))
	
	((_ (v q ...)) (identifier? (syntax v))
	 (syntax (v q ...))) ;; evaluate as function
	
	((_ (v ...))
	 (syntax (pdef-quasi (! v ...))))

	((_ ~)
	 (syntax '~))

	((_ v)
	 (syntax v)))))
  )
