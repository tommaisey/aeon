(library (testing)
  (export test-assert
          test-eqv
          test-list
          test-impl)

  (import (chezscheme)
          (only (srfi s1 lists) list=))

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
  )