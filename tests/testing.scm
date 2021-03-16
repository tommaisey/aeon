(library (testing)
  (export reset-test-results
          print-test-results
          print-test-pass
          test-assert
          test-eq
          test-eq~
          test-impl
          testp)

  (import (chezscheme)
          (only (srfi s1 lists) list=)
          (only (utilities) str+ println repeat inc make-alist)
          (only (event) event-check event-optimise)
          (only (context) context-move context-events-next)
          (only (node-eval) render-arc))

  ;;----------------------------------------------------------
  ;; Set to #t to get printouts for test passes (fails always printed).
  (define print-test-pass (make-parameter #f))

  ;; Prints the accumulated test results. Call 'reset-test-results'
  ;; between runs of the same test suite, or they'll be added together.
  (define (print-test-results)
    (let* ([divider (fold-left str+ "" (repeat 60 "-"))]
           [template "[aeon] tests: [~A pass] [~A fail]"]
           [results (format template tests-passed tests-failed)])
      (println divider results divider)))
  
  (define (reset-test-results)
    (set! tests-passed 0)
    (set! tests-failed 0))

  ;; Internal state for tracking the above.
  (define tests-passed 0)
  (define tests-failed 0)
  (define (register-pass) (set! tests-passed (inc tests-passed)))
  (define (register-fail) (set! tests-failed (inc tests-failed)))

  ;;----------------------------------------------------------
  ;; Base macro for implementing test assertions
  (define-syntax test-impl
    (syntax-rules ()
      ((_ name result-id form check-form message-string)
       (let ([result-id form])
         (if check-form
             (begin
               (register-pass)
               (when (print-test-pass)
                 (println (str+ "Pass: " name)))
               #t)
             (begin
               (register-fail)
               (println (str+ "Fail: " message-string))
               #f))))))

  ;; Test assertions
  (define-syntax test-assert
    (syntax-rules ()
      ((_ name form)
       (test-impl name result form
                  result
                  (format "~A" name)))))
  
  (define-syntax test-eq
    (syntax-rules ()
      ((_ name val form)
       (test-impl name result form
                  (equal? val result)
                  (print-result name val result)))))

  (define-syntax test-eq~
    (syntax-rules ()
      ((_ name val form tolerance)
       (test-impl name result form
                  (<= (abs (- result val)) tolerance)
                  (print-result name val result)))
      ((_ name val form)
       (test-eq~ name val form 0.01))))

  (define-syntax print-result
    (syntax-rules ()
      ((_ name val result)
       (format "'~A'.\nExpected: ~A\nGot: ~A" name val result))))

  ;;----------------------------------------------------------
  ;; Pattern tests
  (define (test-event e expected-values-alist)
    (for-all (lambda (kv) (event-check e (car kv) eqv? (cdr kv)))
              expected-values-alist))

  (define (test-events context expected-values-alists)
    (let ([next-events (context-events-next context)]
          [expected expected-values-alists])
      (if (or (null? next-events) (null? expected))
          (and (null? next-events) (null? expected))
          (and (test-event (car next-events) (car expected))
               (test-events (context-move context 1) (cdr expected))))))

  ;; Test that a pattern has events with the expected values in the arc.
  ;; The events-props form is a list of flattened key/value pairs.
  ;; Note that the result can contain extra key/values to the ones tested,
  ;; but if it's missing any of the specified events-props values it will fail.
  (define-syntax testp
    (syntax-rules ()
      ((_ name arc  pattern-expr ((events-props ...) ...))
       (let ([expected (list (make-alist events-props ...) ...)])
         (test-impl
          name result
          (render-arc pattern-expr arc)
          (test-events result expected)
          (let* ([rendered (context-events-next result)]
                 [cleaned (map (lambda (e) (event-optimise e)) rendered)]
                 [len-expected (length expected)]
                 [len-actual (length cleaned)])
            (if (eqv? len-expected len-actual)
                (format "~A.\nExpected: ~A\nGot: ~A"
                        name expected cleaned)
                (format "~A. Expected ~A events, but got ~A."
                        name len-expected len-actual))))))))
  )
