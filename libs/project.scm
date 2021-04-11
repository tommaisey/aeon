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
        (lambda (p)
          (display ";; Loads the aeon runtime. Don't delete this!" p)
          (newline p)
          (pretty-print code p)
          (newline p)
          (newline p)
          (display ";; You can add project-specific config here:" p)
          (newline p)))
      (call-with-output-file first-file
        (lambda (p)
          (put-datum p '(load "config.scm"))
          (newline p)
          (display ";; Time to make some music..." p)
          (newline p)))
      (call-with-output-file (path+ dir ".dir-locals.el")
        (lambda (p)
          (pretty-print dir-locals p)))
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

  (define dir-locals
    '((nil . ((indent-tabs-mode . nil)))
      (scheme-mode . ((geiser-active-implementations . (chez . nil))))
      (auto-revert-mode . t)))
  
  )
