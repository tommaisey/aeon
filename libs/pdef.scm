#!chezscheme ;; Needed for using weird symbols as names.

;; Defines a macro that is used in values lists for sequences.
;; It attempts to free the user from needing to understand lists,
;; quasiquoting etc. Would all be a lot easier if we have
;; Clojure's [] vector syntax!
(library (pdef)
  (export pdef ^
          ! !2 !3 !4 !5 !6 !7 !8 !9 !10 !11 !12 !13 !14 !15 !16
          tag-pdef-callable
          tag-pdef-not-call)

  (import (chezscheme) (utilities))

  ;;-------------------------------------------------------------------
  ;; ^ Prevents interpreting as a function in pdefs
  ;; ! Represents a repeat of the previous value.
  (declare-keywords ^ !)
  (declare-keywords !2 !3 !4 !5 !6 !7 !8 !9 !10 !11 !12 !13 !14 !15 !16)

  ;;-------------------------------------------------------------------
  ;; Defines a nested pattern, with special symbols for rests, sustains
  ;; and repeats of previous values.
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
      (syntax-case x (^)

        ((_ (^ v ...)) (syntax (resolve-repeats (list (pdef v) ...))))

        ((_ (v q ...))
         (identifier? #'v)
         (lambda (property-lookup)
           (cond
            ((property-lookup #'v #'pdef-call-tag)
             (syntax (v q ...)))
            ((property-lookup #'v #'pdef-not-call-tag)
             (syntax (list v (pdef q) ...)))
            (else
             (syntax (if (procedure? v)
                         (v q ...)
                         (list (pdef v)
                               (pdef q) ...)))))))
        ((_ (v ...))
         (syntax (pdef (^ v ...))))

        ((_ v)
         (syntax v)))))

  ;; Adds extra logic to 'pdef' whereby repeat symbols are
  ;; converted to copies of the previous value.
  (define (resolve-repeats def)
    (define (num-repeats sym)
      (if (not (symbol? sym))
          (values 0)
          (case sym
            [(! !2) 1]
            [!3  2]
            [!4  3]
            [!5  4]
            [!6  5]
            [!7  6]
            [!8  7]
            [!9  8]
            [!10 9]
            [!11 10]
            [!12 11]
            [!13 12]
            [!14 13]
            [!15 14]
            [!16 15]
            [else 0])))
    (let loop ([def def] [out '()])
      (cond
       [(not (unsafe-list? def)) (list def)]
       [(null? def) (reverse out)]
       [else
        (let* ([val (car def)]
               [repeats (num-repeats val)]
               [prev (if (null? out) (list-last def) (car out))]
               [rest (cdr def)])
          (cond
           [(zero? repeats) (loop rest (cons val out))]
           [(eq? 1 repeats) (loop rest (cons prev out))]
           [else (loop rest (push-front (repeat repeats prev) out))]))])))

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

  (tag-pdef-not-call ^ ! !2 !3 !4 !5 !6 !7 !8 !9 !10 !11 !12 !13 !14 !15 !16)

  )
