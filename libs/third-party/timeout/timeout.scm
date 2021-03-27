(library (timeout timeout)
  (export bytes-ready?)
  (import (scheme) (file-tools))

  ;; Loads the dynamic library.
  (define lib
    (let ([dir (caar (library-directories))]
          [so "/third-party/timeout/libtimeout"]
          [ext (case os-symbol
                 ['windows ".dll"]
                 ['macos ".dylib"]
                 ['linux ".so"])])
      (load-shared-object (string-append dir so ext))))

  ;; Takes a file descriptor and a timeout in ms. Returns
  ;; false if the file doesn't become available for reading
  ;; within the timeout period.
  (define bytes-ready?
    (foreign-procedure "bytes_ready" (int int) boolean))
)
