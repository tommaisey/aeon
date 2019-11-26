(library (timeout timeout)
  (export bytes-ready?)
  (import (scheme))

  ;; This isn't nice! Can't figure out how to 
  ;; load this relative to this library directory.
  (define lib (load-shared-object "./libs/third-party/timeout/libtimeout.dylib"))

  ;; Takes a file descriptor and a timeout in ms. Returns
  ;; false if the file doesn't become available for reading
  ;; within the timeout period.
  (define bytes-ready?
    (foreign-procedure "bytes_ready" (int int) boolean))
)