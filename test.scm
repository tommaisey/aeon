(load "runtime/libs.scm")
(import (testing))

;;------------------------------------------------------------------
(reset-test-results)
(load "tests/utilities-tests.scm")
(load "tests/context-tests.scm")
(load "tests/harmony-tests.scm")
(load "tests/basic-ops-tests.scm")
(load "tests/time-ops-tests.scm")
(print-test-results)
