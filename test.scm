(load "runtime/libs.scm")

;;------------------------------------------------------------------
(define (run-tests)
  (load "tests/context-tests.scm")
  (load "tests/harmony-tests.scm")
  (load "tests/basic-ops-tests.scm")
  (load "tests/time-ops-tests.scm"))

(run-tests)

