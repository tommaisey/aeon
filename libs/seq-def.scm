#!chezscheme ;; Needed for using weird symbols as names.

;; Defines a macro that is used in values lists for sequences.
;; It attempts to free the user from needing to understand lists,
;; quasiquoting etc. Would all be a lot easier if we have
;; Clojure's [] vector syntax!
(library (seq-def)
  (export sdef ^ !
          tag-sdef-callable
          tag-sdef-not-call
          seq-marker?
          seq-marker-dur
          seq-marker-val)
  (import (chezscheme) (utilities))

  ;;-------------------------------------------------------------------
  ;; ^ Prevents interpreting as a function in sdefs
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
  ;; (sdef [1 "hi" (+ 2 2) (5 (+ 5 5) !)]) => (1 "hi" 4 (5 10 10))
  ;; (sdef [0 (pick [2 3 ~]) (rnd 0 3)]) => (0 <proc> <proc>)
  (define-syntax sdef
    (lambda (x)
      (syntax-case x (^ * / !)

        ;; * - Subdividing repeats.
        ;; TODO: how to do division? Also, tidal
        ;; can do multiply by fractions. Maybe we
        ;; don't put a new list in the list, but some other
        ;; object representing the speed/slow?
        ((_ (^ v [* n] . rest))
         (syntax
          (let ([vs (sdef v)])
            (sdef [(make-seq-marker vs n) . rest]))))
        ((_ (^ v [*] . rest))
         (syntax (sdef (^ v [* 2] . rest))))
        ((_ (^ v * . rest))
         (syntax (sdef (^ v [* 2] . rest))))

        ;; / - Subdividing repeats
        ((_ (^ v [/ n] . rest))
         (syntax
          (let ([vs (sdef v)])
            (sdef [(make-seq-marker vs (/ 1 n)) . rest]))))
        ((_ (^ v [/] . rest))
         (syntax (sdef (^ v [/ 2] . rest))))
        ((_ (^ v / . rest))
         (syntax (sdef (^ v [/ 2] . rest))))

        ;; ! - Spliced repeats.
        ((_ (^ v [! n] . rest))
         (let* ((n-datum (syntax->datum #'n)))
           (if (> n-datum 1)
               (with-syntax ((n-1 (datum->syntax #'x (sub1 n-datum))))
                 (syntax (sdef (^ v v [! n-1] . rest))))
               (syntax (sdef (^ v . rest))))))
        ((_ (^ v [!] . rest))
         (syntax (sdef (^ v [! 2] . rest))))
        ((_ (^ v ! . rest))
         (syntax (sdef (^ v [! 2] . rest))))

        ;; Build the list recursively.
        ((_ (^ v . rest))
         (syntax (cons (sdef v) (sdef rest))))

        ;; Distinguishing macro & function calls.
        ((_ (v vs ...))
         (identifier? #'v)
         (lambda (property-lookup)
           (cond
            ((property-lookup #'v #'sdef-call-tag)
             (syntax (v vs ...)))
            ((property-lookup #'v #'sdef-not-call-tag)
             (syntax (sdef (^ v vs ...))))
            (else
             (syntax (if (procedure? v)
                         (v vs ...)
                         (sdef (^ v vs ...))))))))

        ;; Base cases
        ((_ ())
         (syntax '()))
        ((_ (v ...))
         (syntax (sdef (^ v ...))))
        ((_ v)
         (syntax v)))))

  ;;-------------------------------------------------------------------
  ;; When a sdef encounters a value that needs to be transformed into
  ;; a further subdivision (e.g. it's followed by (* 2)), it wraps it
  ;; in this object. This can be interpreted by the outer subdividing
  ;; seq.
  (define-immutable-record seq-marker
    [val 1]
    [dur 1])

  ;;------------------------------------------------------------------
  ;; Used for compile-time tagging, see comment to sdef.
  (define sdef-call-tag)
  (define sdef-not-call-tag)

  (define-syntax tag-sdef-callable
    (syntax-rules ()
      ((_ id ...)
       (begin (define-property id sdef-call-tag #t) ...))))

  (define-syntax tag-sdef-not-call
    (syntax-rules ()
      ((_ id ...)
       (begin (define-property id sdef-not-call-tag #t) ...))))

  )
