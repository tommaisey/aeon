#!chezscheme ;; Needed for the extra symbols like »

(library (pdef)
  (export pdef
          ~ » × !
          repeat-sym
          sustain-sym
          rest-sym
          tag-pdef-callable
          tag-pdef-dont-call)

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

  (define-syntax pdef
    (lambda (x)
      (syntax-case x (! ~ » ×)

        ((_ (! v ...)) (syntax (list (pdef v) ...)))
        ((_ (~ v ...)) (syntax (pdef (! ~ v ...))))
        ((_ (» v ...)) (syntax (pdef (! » v ...))))
        ((_ (× v ...)) (syntax (pdef (! × v ...))))

        ((_ (v q ...))
         (identifier? #'v)
         (lambda (property-lookup)
           (cond
             ((property-lookup #'v #'pdef-call-tag)
              (syntax (v q ...)))
             ((property-lookup #'v #'pdef-dont-call-tag)
              (syntax (list v (pdef q) ...)))
             (else
               (syntax (if (procedure? v)
                           (v q ...)
                           (list (pdef v) 
                                 (pdef q) ...)))))))
        ((_ (v ...))
         (syntax (pdef (! v ...))))

        ((_ v)
         (syntax v)))))

  ;;------------------------------------------------------------------
  ;; Used for compile-time tagging, see comment to pdef.
  (define pdef-call-tag)
  (define pdef-dont-call-tag)

  (define-syntax tag-pdef-callable
    (syntax-rules ()
      ((_ id) (define-property id pdef-call-tag #t))))

  (define-syntax tag-pdef-dont-call
    (syntax-rules ()
      ((_ id) (define-property id pdef-dont-call-tag #t))))

  )
