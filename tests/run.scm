(load "runtime/libs.scm")
(import (testing))

;;------------------------------------------------------------------
(reset-test-results)
(parameterize ((source-directories (cons "tests" (source-directories))))
  (load "utilities-tests.scm")
  (load "pdef-tests.scm")
  (load "context-tests.scm")
  (load "harmony-tests.scm")
  (load "basic-ops-tests.scm")
  (load "time-ops-tests.scm")
  (load "version-control-tests.scm"))
(print-test-results)
