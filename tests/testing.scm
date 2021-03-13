(library (testing)
  (export test-assert
          test-eqv
          test-list
          test-impl
          testp)

  (import (chezscheme)
          (only (srfi s1 lists) list=)
          (only (utilities) make-alist)
          (only (event) event-check event-optimise)
          (only (context) context-move context-events-next)
          (only (node-eval) render-arc))

  (define-syntax test-impl
    (syntax-rules ()
      ((_ name result-id form check-form message-string)
       (let ([result-id form])
         (if check-form
             (display (string-append "Pass: " name "\n"))
             (raise-continuable
              (condition
               (make-warning)
               (make-message-condition message-string)
               (make-source-condition 'form))))))))

  (define-syntax test-assert
    (syntax-rules ()
      ((_ name form)
       (test-impl name result form
                  result
                  (format "Fail: ~A" name)))))

  (define-syntax test-eqv
    (syntax-rules ()
      ((_ name val form)
       (test-impl name result form
                  (eqv? val result)
                  (format "Fail: '~A'. Expected ~A, got ~A:" 
                          name val result)))))

  (define-syntax test-list
    (syntax-rules ()
      ((_ name eq-fn val form)
       (test-impl name result form
                  (list= eq-fn val result)
                  (format "Fail: '~A'. Expected ~A, got ~A:" 
                          name val result)))))

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
         (test-impl name result 
          (render-arc pattern-expr arc)
          (test-events result expected)
          (let* ([rendered (context-events-next result)]
                 [cleaned (map (lambda (e) (event-optimise e)) rendered)]
                 [len-expected (length expected)]
                 [len-actual (length cleaned)]
                 [extra-msg (cond
                              ((not (eqv? len-expected len-actual))
                               (format "Expected ~A events, but got ~A."
                                       len-expected len-actual))
                                        (else ""))])
            (string-append "Failed: " name ". " extra-msg "\n"
                           (format "Expected: ~A\nGot:      ~A\nCode:"
                                   expected cleaned))))))))
  )