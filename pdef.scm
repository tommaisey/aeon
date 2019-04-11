#!chezscheme ;; Needed for the extra symbols like »

(library (pdef)
  (export make-pdef-data
	  ~ » × !
	  repeat-sym
	  sustain-sym
	  rest-sym
	  tag-pdef-callable)
  
  (import (scheme) (node-eval))

  (define × '×) ;; Denotes a repeated value in a pdef
  (define » '») ;; Denotes a sustained value in a pdef
  (define ~ '~) ;; Denotes a musical rest in a pdef
  (define ! '!) ;; Prevents a pdef from being evaluated
  (define repeat-sym ×)
  (define sustain-sym »)
  (define rest-sym ~)

  ;;-------------------------------------------------------------------
  ;; Defines a nested pattern, with special symbols for rests, sustains
  ;; and repeats of previous notes.
  ;;
  ;; The tricky part about this is allowing embedded function/macro
  ;; calls whilst not requiring users to understand quasiquoting.
  ;;
  ;; We can detect at runtime whether a first-position identifier is
  ;; a procedure, but for macros we must rely on them being tagged
  ;; using Chez Scheme's define-property, which operates in the compile
  ;; time environment. See:
  ;; http://cisco.github.io/ChezScheme/csug9.5/syntax.html#./syntax:h4
  ;;
  ;; (pdef [1 "hi" (+ 2 2) (5 (+ 5 5))]) => (1 "hi" 4 (5 10))
  ;; (pdef [0 (pick [2 3 ~]) (rnd 0 3)]) => (0 <proc> <proc>)
  ;;
  ;; Additionly there are still cases where we want to place a procedure
  ;; in the first position but not have it evaluated. In this case it's
  ;; necessary to 
  
  (define-syntax make-pdef-data
    (lambda (x)
      (syntax-case x (! ~ » ×)

	((_ (! v ...)) (syntax (list (make-pdef-data v) ...)))
	((_ (~ v ...)) (syntax (make-pdef-data (! ~ v ...))))
	((_ (» v ...)) (syntax (make-pdef-data (! » v ...))))
	((_ (× v ...)) (syntax (make-pdef-data (! × v ...))))
	
	((_ (v q ...))
	 (identifier? (syntax v))
	 (lambda (property-lookup)
	   (if (property-lookup #'v #'pdef-call-tag)
		 (syntax (v q ...))
		 (syntax (if (procedure? v)
			     (v q ...)
			     (list (make-pdef-data v)
				   (make-pdef-data q) ...))))))
	
	((_ (v ...))
	 (syntax (make-pdef-data (! v ...))))

	((_ v)
	 (syntax v)))))


  ;;------------------------------------------------------------------
  ;; Used for compile-time tagging, see comment to pdef.
  (define pdef-call-tag)

  (define-syntax tag-pdef-callable
    (syntax-rules ()
      ((_ id)
       (define-property id pdef-call-tag #t))))
  
  )
