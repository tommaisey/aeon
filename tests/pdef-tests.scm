(module pdef-tests ()

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

  (test-eq "[pdef repeats (!)]"
    '(1 2 2 3) (pdef [1 2 [!] 3]))
  (test-eq "[pdef repeats (! 2)]"
    '(1 2 2 3) (pdef [1 2 [! 2] 3]))
  (test-eq "[pdef repeats (! 3)]"
    '(1 2 2 2 3) (pdef [1 2 (! 3) 3]))
  (test-eq "[pdef repeats (! 2) !]"
    '(1 2 2 2 2 3) (pdef [1 2 (! 3) ! 3]))
  (test-eq "[pdef repeats (! 2) (! 2)]"
    '(1 2 2 2 2 2 3) (pdef [1 2 (! 3) (! 3) 3]))
  (test-eq "[pdef repeats (1 2) (! 3)]"
    '((1 2) (1 2) (1 2) 3) (pdef [[1 2] (! 3) 3]))

  (test-eq "[pdef speed (*)]"
    '(1 (2 2)) (pdef [1 2 *]))
  (test-eq "[pdef speed (* 2)]"
    '(1 (2 2)) (pdef [1 2 (* 2)]))
  (test-eq "[pdef speed (* 3)]"
    '(1 (2 2 2)) (pdef [1 2 (* 3)]))
  (test-eq "[pdef speed (* 3) !]"
    '(1 (2 2 2) (2 2 2)) (pdef [1 2 (* 3) !]))

  (test-eq "[pdef-tag-not-callable]"
    '(~ 1) (pdef [~ 1]))
  (test-eq "[pdef-tag-not-callable]"
    '($ 1) (pdef [$ 1]))

  )
