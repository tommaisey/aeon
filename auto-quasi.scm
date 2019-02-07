(library (auto-quasi)
  (export auto-quasi pdef-quasi ~)
  (import (scheme))
  
  ;;------------------------------------------------------------------
  ;; Allows definition of nested lists without (quasi)quoting.
  ;;
  ;; Lists that start with an identifier are evaluated.
  ;; Lists that don't start with an identifier are just a list.
  ;;
  ;; (auto-quasi (1 2 "hi" (+ 2 2) (5 (+ 5 5)))) => (1 2 "hi" 4 (5 10))
  (define-syntax auto-quasi
    (lambda (x)
      (syntax-case x ()
	
	((_ (v rest ...))
	 (identifier? (syntax v))
	 (syntax (v rest ...))) ; call as lambda
	
	((_ (v ...))
	 (syntax (list (auto-quasi v) ...)))
     
	((_ v)
	 (syntax v)))))

  ;;-------------------------------------------------------------------
  ;; Same as above, but quotes the special character '~', since in the
  ;; context of a pdef it represents a rest.
  (define ~)
  
  (define-syntax pdef-quasi
    (lambda (x)
      (syntax-case x (~)

	((_ (~ rest ...))
	 (syntax (pdef-quasi ('~ rest ...))))
	
	((_ (v rest ...))
	 (identifier? (syntax v))
	 (syntax (v rest ...))) ; call as lambda
	
	((_ (v ...))
	 (syntax (list (pdef-quasi v) ...)))

	((_ ~)
	 (syntax '~))
	
	((_ v)
	 (syntax v)))))
  )
