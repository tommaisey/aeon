(library (system)
  (export run-process make-sc-process build-aeon-libraries)
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

  ;; Builds the rsc3 libraries for e.g. OSC communication.
  (define (build-aeon-libraries)
    (let* ([dir (path-append (current-directory) "libs/third-party/sc3/")]
           [files (directory-list dir)]
           [sls? (lambda (s) (string-suffix? s ".sls"))])
      (when (null? (filter sls? files))
        (let-values ([(exit-code output) (run-process "make")])
          (if (zero? exit-code)
              (println "Successfully built native libraries.")
              (apply println (cons "Error building native libraries:" output)))))))

  ;; Launches a process and returns stdin, stdout and a function
  ;; that can be used to kill the process.
  (define (launch-process command)
    (let* ([proc (process command)]
           [stdout (car proc)]
           [stdin (cadr proc)]
           [pid (caddr proc)]
           [kill (lambda () (system (format "kill -9 ~d" pid)))])
      (values stdout stdin kill pid)))
  
  )
