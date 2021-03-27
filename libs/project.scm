;; Handles creating and managing a new aeon music project.
(library (project)

  (export new-project switch-project)

  (import (chezscheme)
          (utilities)
          (file-tools)
          (version-control)
          (srfi s13 environment ))

  (define (new-project name)
    (let* ([dir  (path+ home-dir "Music/aeon/" name)]
           [first-file (path+ dir "start.scm")]
           [config-file (path+ dir "config.scm")]
           [code (aeon-finder-code)]) ; do early, could error
      (when (file-exists? dir)
        (error 'new-project
          "Can't create project: directory already exists."))
      (mkdir-rec dir)
      (call-with-output-file config-file
        (lambda (p)
          (display ";; You can add project-specific config here:" p)
          (newline p)
          (newline p)
          (display ";; Loads the aeon runtime. Don't delete it!" p)
          (newline p)
          (put-datum p code)))
      (call-with-output-file first-file
        (lambda (p)
          (put-datum p '(load "config.scm"))
          (newline p)
          (display ";; Time to make some music..." p)
          (newline p)))
      (current-directory dir)
      (init-repo)))

  (define (aeon-finder-code)
    `(let ([f ,(get-aeon-file)] [sym 'aeon-file])
       (unless (top-level-bound? sym)
         (set-top-level-value! sym f)
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
  
  )
