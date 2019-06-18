(library (testing)
  (export test-assert
          test-eqv
          test-list)
  
  (import (chezscheme)
          (only (srfi s1 lists) list=))

  (define-syntax test-impl
    (syntax-rules ()
      ((_ name result-id form when-form message-string)
       (let ([result-id form])
         (when when-form
           (raise-continuable
            (condition
             (make-warning)
             (make-message-condition message-string)
             (make-source-condition 'form))))))))

  (define-syntax test-assert
    (syntax-rules ()
      ((_ name form)
       (test-impl name result form
                  (not result) 
                  (format "Test ~A failed: " name)))))

  (define-syntax test-eqv
    (syntax-rules ()
      ((_ name val form)
       (test-impl name result form
                  (not (eqv? val result))
                  (format "Test '~A' failed. Expected ~A, got ~A:" name val result)))))

  (define-syntax test-list
    (syntax-rules ()
      ((_ name eq-fn val form)
       (test-impl name result form
                  (not (list= eq-fn val result))
                  (format "Test '~A' failed. Expected ~A, got ~A:" name val result)))))
  )