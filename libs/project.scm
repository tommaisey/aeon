;; Handles creating and managing a new aeon music project.
(library (project)

  (export new-project)

  (import (chezscheme)
          (utilities)
          (file-tools)
          (version-control))

  (define (new-project name)
    (let* ([dir  (path+ home-dir "Music/aeon/" name)]
           [first-file (path+ dir "start.scm")]
           [config-file (path+ dir "config.scm")]
           [code (aeon-finder-code)])   ; do early, could error
      (when (file-exists? dir)
        (error 'new-project
          "Directory already exists."))
      (mkdir-rec dir)
      (call-with-output-file config-file
        (lambda (port)
          (display ";; Loads the aeon runtime. Don't delete this!" port)
          (newline port)
          (pretty-print code port)
          (newline port)
          (newline port)
          (display ";; You can add project-specific config here:" port)
          (newline port)))
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

  (define (aeon-finder-code)
    `(let ([f ,(get-aeon-file)])
       (unless (top-level-bound? 'aeon-file)
         (set-top-level-value! 'aeon-file f)
         (source-directories (cons (path-parent f)
                                   (source-directories)))
         (load f))))

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
