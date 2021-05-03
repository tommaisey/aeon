(load "runtime/libs.scm")
(import (testing))

;;------------------------------------------------------------------
(reset-test-results)
(parameterize ((source-directories (cons "tests" (source-directories))))
  (load "utilities-tests.scm")
  (load "seq-def-tests.scm")
  (load "context-tests.scm")
  (load "harmony-tests.scm")
  (load "ops-basic-tests.scm")
  (load "ops-time-tests.scm")
  (load "version-control-tests.scm"))
(print-test-results)
