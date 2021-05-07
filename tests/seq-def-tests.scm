(module sdef-tests ()

  (import (chezscheme)
          (testing)
          (seq-def)
          (seq-subdivide))

  (test-eq "[sdef calls function]"
    '(1 2 6) (sdef [1 2 (+ 3 3)]))

  (test-eq "[sdef repeats (!)]"
    '(1 2 2 3) (sdef [1 2 ! 3]))
  (test-eq "[sdef repeats (! !)]"
    '(1 2 2 2 3) (sdef [1 2 ! ! 3]))
  (test-eq "[sdef function and repeats]"
    '(1 2 6 6 6) (sdef [1 2 (+ 3 3) ! !]))

  (test-eq "[sdef repeats (!)]"
    '(1 2 2 3) (sdef [1 2 [!] 3]))
  (test-eq "[sdef repeats (! 2)]"
    '(1 2 2 3) (sdef [1 2 [! 2] 3]))
  (test-eq "[sdef repeats (! 3)]"
    '(1 2 2 2 3) (sdef [1 2 (! 3) 3]))
  (test-eq "[sdef repeats (! 2) !]"
    '(1 2 2 2 2 3) (sdef [1 2 (! 3) ! 3]))
  (test-eq "[sdef repeats (! 2) (! 2)]"
    '(1 2 2 2 2 2 3) (sdef [1 2 (! 3) (! 3) 3]))
  (test-eq "[sdef repeats (1 2) (! 3)]"
    '((1 2) (1 2) (1 2) 3) (sdef [[1 2] (! 3) 3]))

  (test-assert "[sdef speed (*)]"
    (seq-marker? (list-ref (sdef [1 2 *]) 1)))
  (test-assert "[sdef speed (* 2)]"
    (seq-marker? (list-ref (sdef [1 2 (* 2)]) 1)))
  (test-eq "[sdef speed (* 3)]"
    3 (seq-marker-dur (list-ref (sdef [1 2 (* 3)]) 1)))
  (test-eq "[sdef speed (* 3) !]"
    3 (seq-marker-dur (list-ref (sdef [1 2 (* 3) !]) 2)))

  (test-eq "[sdef-tag-not-callable]"
    '(~ 1) (sdef [~ 1]))
  (test-eq "[sdef-tag-not-callable]"
    '($ 1) (sdef [$ 1]))

  )
