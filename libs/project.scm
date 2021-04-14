;; Handles creating and managing a new aeon music project.
(library (project)

  (export new-project projects-dir global-config-file)

  (import (chezscheme)
          (utilities)
          (file-tools)
          (version-control))

  ;; A Chez parameter pointing to a directory for aeon projects.
  (define projects-dir
    (make-parameter (path+ home-dir "Music/aeon/")))

  ;; A Chez parameter pointing to a location for a global config file.
  (define global-config-file
    (make-parameter (path+ (projects-dir) "config.scm")))

  ;; Creates a new project directory, with code to load aeon and
  ;; your global config file. Also initialises a git repo for saving.
  (define (new-project name)
    (let* ([dir (path+ (projects-dir) name)]
           [first-file (path+ dir "main.scm")]
           [local-config-file (path+ dir "config.scm")]
           [find-aeon-code (aeon-finder-code)] ;; do early, could error
           [find-config-code (config-finder-code)])
      (when (file-exists? dir)
        (error 'new-project
          "Directory already exists."))
      (mkdir-rec dir)
      (call-with-output-file local-config-file
        (lambda (port)
          (display ";; Loads the aeon runtime:\n" port)
          (pretty-print find-aeon-code port)
          (display "\n;; Loads your global config:\n" port)
          (pretty-print find-config-code port)
          (display "\n;; You can add project-specific config here:\n" port)))
      (call-with-output-file first-file
        (lambda (port)
          (put-datum port '(load "config.scm"))
          (newline port)
          (display ";; Time to make some music..." port)
          (newline port)))
      (call-with-output-file (path+ dir ".dir-locals.el")
        (lambda (port)
          (display dir-locals port)))
      (current-directory dir)
      (init-repo)))

  ;;-------------------------------------------------------------
  (define (aeon-finder-code)
    `(let ([f ,(get-aeon-file)])
       (unless (top-level-bound? 'aeon-file)
         (set-top-level-value! 'aeon-file f)
         (source-directories (cons (path-parent f)
                                   (source-directories)))
         (load f))))

  (define (config-finder-code)
    `(when (file-exists? (global-config-file))
       (load (global-config-file))))

  (define (get-aeon-file)
    (if (top-level-bound? 'aeon-file)
        (top-level-value 'aeon-file)
        (let ([f (path+ (current-directory) "aeon.scm")])
          (if (file-exists? f)
              (begin (set-top-level-value! 'aeon-file f) (values f))
              (error 'new-project "Can't find aeon.scm")))))

  ;; Sadly Scheme's put-datum procedures don't output dots in
  ;; between the mode name and variables list, which Emacs wants.
  (define dir-locals
    "((nil . ((indent-tabs-mode . nil)))
 (scheme-mode . ((geiser-active-implementations . (chez . nil))))
 (auto-revert-mode . t))\n")

  )
