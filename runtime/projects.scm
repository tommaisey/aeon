;; Clears all running patterns and the loads the project's main
;; file, which will usually restart its patterns afresh.
(define (reinit-project)
  (if (file-exists? "main.scm")
      (let ([j jump])
        (set! jump (lambda args '()))
        (stop)
        (load "main.scm")
        (set! jump j))
      (println "No active aeon project. Try: (switch-project \"name\")")))

(define (jump . args)
  (apply vc:jump args)
  (reinit-project))

(define (switch-project name)
  (let ([dir (path+ (projects-dir) name)])
    (unless (file-directory? dir)
      (error 'switch-project "No project found" dir))
    (current-directory dir)
    (reinit-project)))
