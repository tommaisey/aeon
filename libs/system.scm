;; Some procedures for setting up and running the system,
;; for example running shell commands, running SuperCollider
;; in particular, and compiling some libraries that we use.
(library (system)
  (export run-command
          run-commands
          launch-process
          launch-supercollider
          build-aeon-libraries
          clean-aeon-libraries
          syscall-file-prefix)
  
  (import (chezscheme) (utilities) (file-tools))

  ;; Runs a process, blocking until it completes.
  ;; => (values exit-code output-lines)
  (define (run-command command)
    (let* ([file-name (syscall-file-name)]
           [file (path-append (current-directory) file-name)]
           [exit-code (system (format "~a > ~s 2>&1" command file))]
           [lines (file-lines file)])
      (delete-file file)
      (values exit-code lines)))

  ;; Runs a list of processes, one after the other, bailing
  ;; out if any of them fail, and returning its error message.
  ;; => (values success? error-string)
  (define (run-commands . cmds)
    (let loop ([cmds cmds])
      (if (null? cmds)
          (values #t "done")
          (let-values ([(exit-code txt) (run-command (car cmds))])
            (if (zero? exit-code)
                (loop (cdr cmds))
                (values #f txt))))))

  ;; Launches a process and returns ports to read/write to it.
  ;; => (values stdin stdout kill-process-fn pid)
  (define (launch-process command)
    (let* ([sc (process command)]
           [stdout (car sc)]
           [stdin (cadr sc)]
           [pid (caddr sc)]
           [kill (lambda () (system (format "kill -9 ~d" pid)))])
      (values stdout stdin kill pid)))

  ;;-------------------------------------------------------------------
  ;; A prefix for files where system call output will be directed.
  ;; These files should get cleaned up automatically.
  (define (syscall-file-name)
    (str+ syscall-file-prefix (number->string (random 9999))))
  
  (define syscall-file-prefix ".aeon-syscall")

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
  ;; SC output that keeps returning => (values :event text)
  ;; => (values (reader-fn) => (values :event text)
  ;;            (kill-fn) => #void)
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

  ;;-------------------------------------------------------------------------
  ;; Building the libraries required by aeon.
  (define (third-dir-from root)
    (path+ root "libs/third-party/"))
  (define (sc3-dir-from root)
    (path+ root "libs/third-party/sc3"))

  (define (aeon-libraries-missing? aeon-root-dir)
    (let ([sc3 (sc3-dir-from aeon-root-dir)])
      (null? (filter (has-extension? "sls") (directory-list sc3)))))

  (define (rohan-drake-mk dir dest-dir)
    (parameterize ([source-directories (cons dir (source-directories))]
                   [current-directory dir]
                   [command-line (list "mk.scm" dest-dir)])
      (load (path+ dir "mk.scm"))))

  (define (c-mk libname c-files)
    (let* ([fs (fold-left (lambda (a b) (str+ a " " b)) "" c-files)]
           [run (lambda (fmt-str) (system (format fmt-str libname fs)))])
      (case os-symbol
        ['macos (run "cc -dynamiclib -o ~a.dylib ~a")]
        [['linux 'freebsd] (run "cc -fPIC -shared -o ~a.so ~a")]
        [else (error 'build-aeon-libraries
                "Can only build libraries on linux and macOS.")])))

  ;; Builds the rsc3 libraries for e.g. OSC communication.
  (define (build-aeon-libraries aeon-root-dir)
    (let* ([third-dir (third-dir-from aeon-root-dir)]
           [sc3-dir (sc3-dir-from aeon-root-dir)]
           [sc3-sub (lambda (sub) (path+ sc3-dir sub))]
           [sc3-make (sc3-sub "mk-r6rs")]
           [lib-dirs (cons sc3-make (library-directories))])
      (when (aeon-libraries-missing? aeon-root-dir)
        (parameterize ([library-directories lib-dirs])
          (rohan-drake-mk (sc3-sub "rhs/mk")  sc3-dir)
          (rohan-drake-mk (sc3-sub "sosc/mk") sc3-dir)
          (rohan-drake-mk (sc3-sub "rsc3/mk") sc3-dir))
        (parameterize ([current-directory (path+ third-dir "timeout")])
          (c-mk "libtimeout" (list "timeout.c"))))))
  
  ;; Clean the build scheme and c libraries out.
  (define (clean-aeon-libraries aeon-root-dir)
    (let* ([del? (has-extension? "sls" "so" "dylib")]
           [sc3-dir (sc3-dir-from aeon-root-dir)]
           [sc3-files (filter del? (child-file-paths sc3-dir))]
           [timeout-dir (path+ (third-dir-from aeon-root-dir) "timeout")]
           [timeout-files (filter del? (child-file-paths timeout-dir))])
      (and
       (for-all identity (map delete-file timeout-files))
       (for-all identity (map delete-file sc3-files)))))
  
  )
