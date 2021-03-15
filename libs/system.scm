(library (system)
  (export run-process make-sc-process)
  (import (chezscheme) (utilities))

  ;; Launches a process and returns stdin, stdout and a function
  ;; that can be used to kill the process.
  (define (run-process command)
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
                  (run-process (format "~a -u ~a -V ~a" path port verbosity))]
                 [(old-exit-handler) (exit-handler)])
      ;; kill scsynth when scheme exits
      ;; TODO: doesn't work when we invoke `scheme aeon.scm`
      (exit-handler (lambda args (kill) (apply old-exit-handler args)))
      (println (format "~a pid: ~d" (string-last path 32 "...") pid))
      (lambda () (let* ([result (get-line stdout)]
                   [said (lambda (x) (string-contains-ci result x))])
              (cond
               [(eof-object? result)
                (begin (println "scsynth quit.")
                       (exit-handler old-exit-handler)
                       #f)]
               [(or (said "error") (said "exception"))
                (begin (println result) (kill) #f)]
               [(said "late")
                (println result)]
               [else #t])))))
  
  )
