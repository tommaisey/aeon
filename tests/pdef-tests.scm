(module subdivide-tests ()

  (import (chezscheme)
          (testing)
          (pdef)
          (nodes-subdivide))

  (test-eq "[pdef calls function]"
    '(1 2 6) (pdef [1 2 (+ 3 3)]))

  (test-eq "[pdef repeats (!)]"
    '(1 2 2 3) (pdef [1 2 ! 3]))
  (test-eq "[pdef repeats (! !)]"
    '(1 2 2 2 3) (pdef [1 2 ! ! 3]))
  (test-eq "[pdef function and repeats]"
    '(1 2 6 6 6) (pdef [1 2 (+ 3 3) ! !]))

  (test-eq "[pdef repeats (!2)]"
    '(1 2 2 3) (pdef [1 2 !2 3]))
  (test-eq "[pdef repeats (!4)]"
    '(1 2 2 2 2 3) (pdef [1 2 !4 3]))

  (test-eq "[pdef-tag-not-callable]"
    '(~ 1) (pdef [~ 1]))
  (test-eq "[pdef-tag-not-callable]"
    '($ 1) (pdef [$ 1]))

  )
