;; Some procedures for setting up and running the system,
;; for example running shell commands, running SuperCollider
;; in particular, and compiling some libraries that we use.
(library (system)
  (export run-process
          launch-process
          launch-supercollider
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

  ;; Launches a process and returns stdin, stdout and a
  ;; function that can be used to kill the process.
  (define (launch-process command)
    (let* ([sc (process command)]
           [stdout (car sc)]
           [stdin (cadr sc)]
           [pid (caddr sc)]
           [kill (lambda () (system (format "kill -9 ~d" pid)))])
      (values stdout stdin kill pid)))

  ;;-------------------------------------------------------------------
  ;; Launches a SuperCollider sub-process.
  ;; It first blocks waiting for scsynth to boot.
  ;; If that fails, it returns #f.
  ;; If it succeeds, it launches a thread to keep reading scsynth's output.
  ;; In this case the thread id is returned.
  (define (launch-supercollider port possible-paths)
    (define (thread-loop sc kill)
      (let-values ([(event text) (sc)])
        (case event
          [:error (begin (println text) (kill))]
          [:quit  (begin (println "scsynth quit.") (kill))]
          [:late  (begin (println text) (thread-loop sc kill))]
          [else   (thread-loop sc kill)])))
    (define (block-loop sc kill max-lines-before-quit)
      (if (zero? max-lines-before-quit)
          (begin (println "scsynth took too long.") (kill) #f)
          (let-values ([(event text) (sc)])
            (case event
              [:ready (fork-thread (lambda () (thread-loop sc kill)))]
              [:error (begin (println text) (kill) #f)]
              [:quit  (begin (println "scsynth quit.") (kill) #f)]
              [else   (block-loop sc kill (dec max-lines-before-quit))]))))
    (let ([path (find sc-responds? possible-paths)])
      (if (not path)
          (begin (println "Couldn't find scsynth.") #f)
          (let-values ([(sc kill) (make-sc-process path port)])
            (block-loop sc kill 500)))))

  ;; Keywords returned by make-sc-process as it scans SC's output.
  (declare-keywords :quit :late :error :ready :other)
  
  ;; Runs a SuperCollider process using the executable at the given
  ;; path and using the given port. Returns a procedure for reading
  ;; SC output that keeps returning #t as long as SC is alive, and
  ;; #f if it crashes or fails to boot correctly.
  (define (make-sc-process path port)
    (let*-values ([(cmd) (format "~a -u ~a" path port)]
                  [(stdout stdin kill pid) (launch-process cmd)]
                  [(old-exit-handler) (exit-handler)])
      ;; kill scsynth when scheme exits
      ;; TODO: doesn't work when we invoke `scheme aeon.scm`
      (exit-handler (lambda args (kill) (apply old-exit-handler args)))
      (println (format "~a pid: ~d" (string-last path 32 "...") pid))
      (values
        (lambda () (let* ([text (get-line stdout)]
                     [said? (lambda (x) (string-contains-ci text x))])
                (values
                  (cond
                   [(eof-object? text) :quit]
                   [(said? "late") :late]
                   [(or (said? "error") (said? "exception")) :error]
                   [(said? "server ready") :ready]
                   [else :other])
                  text)))
        (lambda () (kill) (exit-handler old-exit-handler)))))

  (define (sc-responds? path)
    (and (file-exists? path)
         (zero? (system (format "~s -v > /dev/null 2>&1" path)))))

  ;;----------------------------------------------------------------------------------
  ;; Building the libraries required by aeon.
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
