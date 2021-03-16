;; Some procedures for setting up and running the system,
;; for example running shell commands, running SuperCollider
;; in particular, and compiling some libraries that we use.
(library (system)
  (export run-process
          launch-process
          make-sc-process
          build-aeon-libraries
          clean-aeon-libraries)
  
  (import (chezscheme) (utilities) (file-tools))

  ;; Runs a process in a blocking way, returning the exit
  ;; code and output (as a string).
  (define (run-process command)
    (let* ([file-name (str+ "." (number->string (random 9999)))]
           [file (path-append (current-directory) file-name)]
           [exit-code (system (format "~a > ~s" command file))]
           [lines (file-lines file)])
      (delete-file file)
      (values exit-code lines)))

  ;; Launches a process and returns stdin, stdout and a function
  ;; that can be used to kill the process.
  (define (launch-process command)
    (let* ([proc (process command)]
           [stdout (car proc)]
           [stdin (cadr proc)]
           [pid (caddr proc)]
           [kill (lambda () (system (format "kill -9 ~d" pid)))])
      (values stdout stdin kill pid)))

  ;; Runs a SuperCollider process using the executable at the given
  ;; path and using the given port. Returns a procedure for reading
  ;; SC output that keeps returning #t as long as SC is alive, and
  ;; #f if it crashes or fails to boot correctly.
  ;; Use a verbosity of 0 for full scsynth output
  (define* (make-sc-process path port [/opt (verbosity -2)])
    (let-values ([(stdout stdin kill pid)
                  (launch-process (format "~a -u ~a -V ~a" path port verbosity))]
                 [(old-exit-handler) (exit-handler)])
      ;; kill scsynth when scheme exits
      ;; TODO: doesn't work when we invoke `scheme aeon.scm`
      (exit-handler (lambda args (kill) (apply old-exit-handler args)))
      (println (format "~a pid: ~d" (string-last path 32 "...") pid))
      (lambda () (let* ([result (get-line stdout)]
                   [said? (lambda (x) (string-contains-ci result x))])
              (cond
               [(eof-object? result)
                (begin (println "scsynth quit.")
                       (exit-handler old-exit-handler)
                       #f)]
               [(or (said? "error") (said? "exception"))
                (begin (println result) (kill) #f)]
               [(said? "late")
                (begin (println result) #t)]
               [else #t])))))

  ;;----------------------------------------------------------------------------------
  ;; Functions for building the libraries required by aeon.
  (define (third-dir-from root)
    (path-append root "libs/third-party/"))
  (define (sc3-dir-from root)
    (path-append root "libs/third-party/sc3"))

  (define (aeon-libraries-missing? aeon-root-dir)
    (let ([sc3 (sc3-dir-from aeon-root-dir)]
          [sls? (lambda (s) (string-suffix? ".sls" s))])
      (null? (filter sls? (directory-list sc3)))))

  (define (rohan-drake-mk dir dest-dir)
    (parameterize ([source-directories (cons dir (source-directories))]
                   [current-directory dir]
                   [command-line (list "mk.scm" dest-dir)])
      (load (path-append dir "mk.scm"))))

  (define (c-mk libname c-files)
    (let ([f (fold-left (lambda (a b) (str+ a " " b)) "" c-files)])
      (case os-symbol
        ['macos (system (format "cc -dynamiclib -o ~a.dylib ~a" libname f))]
        [('linux 'freebsd) (system (format "cc -fPIC -shared -o ~a.so ~a" libname f))]
        [else (error 'build-aeon-libraries "Can only build libraries on linux and macOS.")])))

  ;; Builds the rsc3 libraries for e.g. OSC communication.
  (define (build-aeon-libraries aeon-root-dir)
    (let* ([third-dir (third-dir-from aeon-root-dir)]
           [sc3-dir (sc3-dir-from aeon-root-dir)]
           [sc3-sub (lambda (sub) (path-append sc3-dir sub))]
           [sc3-make (sc3-sub "mk-r6rs")]
           [lib-dirs (cons sc3-make (library-directories))])
      (when (aeon-libraries-missing? aeon-root-dir)
        (parameterize ([library-directories lib-dirs])
          (rohan-drake-mk (sc3-sub "rhs/mk")  sc3-dir)
          (rohan-drake-mk (sc3-sub "sosc/mk") sc3-dir)
          (rohan-drake-mk (sc3-sub "rsc3/mk") sc3-dir))
        (parameterize ([current-directory (path-append third-dir "timeout")])
          (c-mk "libtimeout" (list "timeout.c"))))))

  (define (clean-aeon-libraries aeon-root-dir)
    (let* ([third-dir (third-dir-from aeon-root-dir)]
           [sc3-dir (sc3-dir-from aeon-root-dir)]
           [sls? (lambda (s) (string-suffix? ".sls" s))])
      (for-each delete-file (filter sls? (directory-list sc3-dir)))
      (parameterize ([current-directory (path-append third-dir "timeout")])
        (delete-file "timeout.so")
        (delete-file "libtimeout.dylib")
        (delete-file "libtimeout.so"))
      #t))
  
  )
