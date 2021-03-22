(load "runtime/libs.scm")
(import (testing))

;;------------------------------------------------------------------
(reset-test-results)
(load "tests/utilities-tests.scm")
(load "tests/pdef-tests.scm")
(load "tests/context-tests.scm")
(load "tests/harmony-tests.scm")
(load "tests/basic-ops-tests.scm")
(load "tests/time-ops-tests.scm")
(load "tests/version-control-tests.scm")
(print-test-results)
