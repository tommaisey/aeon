(module version-control-tests ()

  (import (chezscheme)
          (testing)
          (file-tools)
          (version-control))

  (define (write-file name text)
    (call-with-output-file name
        (lambda (p) (put-string p text))))

  (define test-dir "/tmp/aeon-test")
  (rmdir test-dir)
  (mkdir test-dir)

  (parameterize ([current-directory test-dir])

    (test-assert "initialize repo"
      (nth-value 0 (init-repo)))
    (test-assert "initialize repo again (does nothing)"
      (nth-value 0 (init-repo)))
    (test-assert "has .gitignore"
      (file-exists? (path+ test-dir ".gitignore")))
    (test-eq "has branch 'main'" '("main") (list-saves))
    (test-eq "on branch 'main'" "main" (current-branch))

    ;; Saving to an existing branch name
    (write-file "f1.txt" "hi")
    (test-eq "save is successful (no name)" #t (nth-value 0 (save)))
    (test-eq "branch is called 'main'" '("main") (list-saves))

    ;; Make another save so we have somewhere to jump to:
    (write-file "f2.txt" "hi")
    (save)

    ;; Jumping back and forward
    (test-eq "jump back returns changed files" '("f2.txt") (jump -1))
    (test-eq "jump fwrd returns changed files" '("f2.txt") (jump +1))
    (test-eq "jump selects branch 'main'" "main" (current-branch))

    (test-eq "jump back is limited to second commit" 1
             (begin (jump -7) (length (commits-back))))
    (test-eq "jump fwrd is limited to second commit" 2
             (begin (jump 10) (length (commits-back))))
    (test-eq "jump selects branch 'main'" "main" (current-branch))

    ;; Saving to a new branch name
    (write-file "f3.txt" "hi")
    (test-eq "save is successful (with name)" #t (nth-value 0 (save "yabba")))
    (test-eq "new branch is added" '("yabba" "main") (list-saves))
    (test-eq "on branch 'yabba'" "yabba" (current-branch))

    ;; Jumping to a named branch
    (test-eq "jump named returns changed files" '("f3.txt") (jump "main"))

    ;; Saving to a new branch name
    (write-file "f4.txt" "hi")
    (test-eq "save an offshoot branch" #t (nth-value 0 (save "dabba")))
    (test-eq "on branch 'dabba'" "dabba" (current-branch))
    (test-eq "list-saves puts current branch at the top"
      '("dabba" "main" "yabba") (list-saves))

    )

  (rmdir test-dir))
