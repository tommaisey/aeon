#!chezscheme ;; Needed for the extra symbols like »

(library (pdef)
  (export pdef ! • » × pdef-node-tag)
  (import (scheme) (node-eval))

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
  ;; (pdef [0 (pick [2 3 •]) (rnd 0 3)]) => (0 <proc> <proc>)
  
  (define-syntax pdef
    (lambda (x)
      (syntax-case x (! » ×)

	((_ (× v n))
	 (syntax (build-splicer '× v n)))

	((_ (» v n))
	 (syntax (build-splicer '» v n)))

	((_ (! v ...))
	 (syntax (list (pdef v) ...)))
	
	((_ (v q ...)) (identifier? (syntax v))
	 (lambda (property-lookup)
	   (if (property-lookup #'v #'pdef-node-tag)
		 (syntax (v q ...))
		 (syntax (if (procedure? v)
			     (v q ...)
			     (list (pdef v) (pdef q) ...))))))
	
	((_ (v ...))
	 (syntax (list (pdef v) ...)))

	((_ v)
	 (syntax v)))))

  ;;------------------------------------------------------------------
  ;; We tag certain identifiers with this using Chez Scheme's define-property.
  ;; Primarily used to allow macro calls inside pdefs.
  (define pdef-node-tag)

  ;; This is used to 'escape' a pdef list, in the case where we want a list
  ;; of procedures, rather than calling the procedure in the first position.
  (define !)
  
  )
