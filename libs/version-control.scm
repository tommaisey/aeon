;; This library provides a highly simplified and streamlined interface
;; to the `git` version control system, for use during live performance.
;; It needs to work with `git` version 2.20.1, because that's installed
;; on a lot of still-not-that-old Macs.
(library (version-control)
  (export save
          jump
          list-saves
          print-saves
          init-repo
          current-branch)
  
  (import (chezscheme) (utilities) (file-tools) (system))

  ;; Save the current state of the project, optionally in a new branch.
  ;; => (values success? error-msg)
  (define* (save [/opt branch-name])
    (when branch-name
      (when (string-contains branch-name " ")
        (error 'save "branch cannot contain contain spaces" branch-name))
      (run-command (format "git checkout -b ~a" branch-name)))
    (commit "-")) ;; TODO: message?

  ;; Jump by a number of commits back (negative) forward (positive)
  ;; or to a specific branch/hash (string).
  ;; If jumping forwards you may specify a branch to jump towards.
  ;; If you don't, it will jump towards the newest branch.
  ;; => (files-that-changed ...)
  (define* (jump dest [/opt target-branch-forwards])
    (cond
     [(string? dest)  (jump-branch dest)]
     [(integer? dest) (jump-by dest target-branch-forwards)]
     [else (error 'jump "requires an integer or string, got" dest)]))

  ;; Returns a list of the repo's branches, with the current
  ;; branch (if applicable) at the front.
  (define (list-saves)
    (define (first? a b) (or (string-contains a "* ") (string-ci<? a b)))
    (filter-branches (list-sort first? (lines-output "git branch"))))

  ;; Prints the branches in a tree view.
  (define (print-saves)
    (define cmd
      (str+ "git log --graph --decorate --all "
            "--date=relative "
            "--pretty='format:[%ad] %D'"))
    (define (trans s)
      (let ([pos (string-contains-ci s "HEAD")])
        (or (and pos (string-replace s pos (+ pos 4) "x")) s)))
    (apply println (map trans (lines-output cmd))))

  ;; Initialize a git repo in (current-directory).
  ;; If one already exists, then nothing is done.
  ;; => (values success? (output ...))
  (define (init-repo)
    (let-values ([(exit-code output) (run-command "git init")])
      (if (zero? exit-code)
          (if (git-reinitialized? output)
              (values #t 'done)
              (first-commit))
          (values #f output))))

  ;;-------------------------------------------------------------------
  (define (current-branch)
    (single-output "git rev-parse --abbrev-ref HEAD"))

  (define (current-commit)
    (single-output "git rev-parse HEAD"))

  ;; Includes HEAD
  (define (commits-back)
    (lines-output "git rev-list HEAD"))

  ;; Includes HEAD (added for consistency with commits-back)
  (define (commits-forward branch)
    (let ([cmd "git rev-list --reverse --ancestry-path HEAD...~a"])
      (cons (current-commit) (lines-output (format cmd branch)))))

  (define (changed-files ref)
    (lines-output (format "git diff --name-only HEAD ~a" ref)))

  (define (newest-branches)
    (define fn (compose lines-output filter-branches reverse))
    (fn "git branch --sort=authordate"))

  ;;-------------------------------------------------------------------
  ;; Make the first commit to a freshly initalized repo.
  (define (first-commit)
    (call-with-output-file ".gitignore"
      (lambda (port) (put-string port gitignore)))
    (run-commands
     "git add .gitignore"
     "git commit -m \"aeon init\""
     ;; Rename the branch: (git 2.20.1 compatibile)
     "git branch -m $(git rev-parse --abbrev-ref HEAD) main"))

  ;; Make subsequent commits to the repo
  (define (commit msg-string)
    (let ([prev-commit (current-commit)])
      (run-commands
       "git add --all"
       (format "git commit -m ~s" msg-string))))

  ;; Jump directly to a branch
  (define (jump-branch name)
    (let ([prev-commit (current-commit)])
      (run-command (format "git checkout ~a" name))
      (printfln "Jumped to ~a" name)
      (changed-files prev-commit)))

  ;; Jump n commits back (negative) or forward (positive).
  ;; Since there may be many branches in the forward direction,
  ;; we need to specify a branch to jump 'towards'.
  ;; If #f is supplied, we jump to the newest.
  (define (jump-by n end-ref)
    (let* ([txt (if (> n 0) "forward" "back")]
           [end-ref (or end-ref (car (newest-branches)))]
           [commits (if (> n 0)
                        (commits-forward end-ref)
                        (commits-back))]
           [num (min (abs n) (dec (length commits)))]
           [prev-commit (current-commit)])
      (if (> num 0)
          (let ([hash (list-ref commits num)])
            (run-command (format "git checkout ~a" hash))
            (printfln "Jumped ~a ~a" txt num)
            (changed-files prev-commit))
          (begin (println "Can't jump any further.") '()))))

  ;;-------------------------------------------------------------------
  ;; A standard .gitignore file's contents.
  (define gitignore
    (str+ syscall-file-prefix "*\n*.DS_Store\n*~\n"))

  ;; Whether the response to `git init` indicates an existing git repo.
  (define (git-reinitialized? init-output-lines)
    (for-any (lambda (s) (string-contains-ci s "reinitialized"))
             init-output-lines))

  ;; Runs a command. If it fails, returns the default.
  ;; If it succeeds, it passes the output lines to a hander fn.
  (define (output-or cmd default handle-lines-fn)
    (call-with-values
      (lambda () (run-command cmd))
      (lambda (exit-code lines)
        (if (and (zero? exit-code) (pair? lines))
            (handle-lines-fn lines)
            (values default)))))
  
  (define (lines-output cmd)
    (output-or cmd '() identity))

  (define (single-output cmd)
    (output-or cmd "" car))

  ;; Filters a list produced by `git branch`. Removes a HEAD entry, and
  ;; trims away the spacing and asterisk marking the current branch.
  (define (filter-branches branches)
    (define (ref? b) (not (string-contains b "HEAD detached")))
    (define (trim b) (substring b 2 (string-length b)))
    (map trim (filter ref? branches)))

  )
