#!chezscheme ;; Needed for using weird symbols as names.

;; Defines a macro that is used in values lists for sequences.
;; It attempts to free the user from needing to understand lists,
;; quasiquoting etc. Would all be a lot easier if we have
;; Clojure's [] vector syntax!
(library (pdef)
  (export pdef ^ !
          tag-pdef-callable
          tag-pdef-not-call)

  (import (chezscheme) (utilities))

  ;;-------------------------------------------------------------------
  ;; ^ Prevents interpreting as a function in pdefs
  ;; ! Represents a repeat of the previous value.
  (declare-keywords ^ !)

  ;;-------------------------------------------------------------------
  ;; This hairy beast implements something similar to Tidal Cycle's
  ;; 'mininotation', where a list can be created with special syntax
  ;; for repeating/subbdiving values. The user is freed from writing
  ;; (list) or quasiquoting as far as possible.
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
  ;; (pdef [1 "hi" (+ 2 2) (5 (+ 5 5) !)]) => (1 "hi" 4 (5 10 10))
  ;; (pdef [0 (pick [2 3 ~]) (rnd 0 3)]) => (0 <proc> <proc>)
  (define-syntax pdef
    (lambda (x)
      (syntax-case x (^ * !)

        ;; * - Subdividing repeats.
        ;; TODO: how to do division? Also, tidal
        ;; can do multiply by fractions. Maybe we
        ;; don't put a new list in the list, but some other
        ;; object representing the speed/slow?
        ((_ (^ v [* n] . rest))
         (syntax
          (let ((v2 (pdef v)))
            (pdef (^ (repeat n v2) . rest)))))
        ((_ (^ v [*] . rest))
         (syntax (pdef (^ v [* 2] . rest))))
        ((_ (^ v * . rest))
         (syntax (pdef (^ v [* 2] . rest))))

        ;; ! - Spliced repeats.
        ((_ (^ v [! n] . rest))
         (let* ((n-datum (syntax->datum #'n)))
           (if (> n-datum 1)
               (with-syntax ((n-1 (datum->syntax #'x (sub1 n-datum))))
                 (syntax (pdef (^ v v [! n-1] . rest))))
               (syntax (pdef (^ v . rest))))))
        ((_ (^ v [!] . rest))
         (syntax (pdef (^ v [! 2] . rest))))
        ((_ (^ v ! . rest))
         (syntax (pdef (^ v [! 2] . rest))))

        ;; Build the list recursively.
        ((_ (^ v . rest))
         (syntax (cons (pdef v) (pdef rest))))

        ;; Distinguishing macro & function calls.
        ((_ (v vs ...))
         (identifier? #'v)
         (lambda (property-lookup)
           (cond
            ((property-lookup #'v #'pdef-call-tag)
             (syntax (v vs ...)))
            ((property-lookup #'v #'pdef-not-call-tag)
             (syntax (pdef (^ v vs ...))))
            (else
             (syntax (if (procedure? v)
                         (v vs ...)
                         (pdef (^ v vs ...))))))))

        ;; Base cases
        ((_ ())
         (syntax '()))
        ((_ (v ...))
         (syntax (pdef (^ v ...))))
        ((_ v)
         (syntax v)))))

  ;;------------------------------------------------------------------
  ;; Used for compile-time tagging, see comment to pdef.
  (define pdef-call-tag)
  (define pdef-not-call-tag)

  (define-syntax tag-pdef-callable
    (syntax-rules ()
      ((_ id ...)
       (begin (define-property id pdef-call-tag #t) ...))))

  (define-syntax tag-pdef-not-call
    (syntax-rules ()
      ((_ id ...)
       (begin (define-property id pdef-not-call-tag #t) ...))))

  )
