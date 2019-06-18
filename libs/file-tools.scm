(library (file-tools)

  (export valid-scheme-path?
          path-append
          child-file-paths
          definitions-in-file)

  (import (chezscheme)
          (only (srfi s13 strings) string-suffix?))

  ;; Doesn't check it exists, just for an apparently valid path.
  (define (valid-scheme-path? path)
    (string=? "scm" (path-extension path)))

  ;; A safer way to add a file name to a directory
  (define (path-append dir file)
    (let* ([sep (string (directory-separator))]
           [dir (if (string-suffix? sep dir) dir (string-append dir sep))])
      (string-append dir file)))

  ;; Unlike directory list, returns full path of child files
  (define (child-file-paths dir)
    (map (lambda (f) (path-append dir f))
         (directory-list dir)))

  ;; Looks for top-level forms in a file matching defining-form? and
  ;; returns the name of the definition using a name-getter fn.
  ;; Warning: currently won't work with definitions in a library or module.
  (define (definitions-in-file file-path defining-form? name-getter)
    (define (do-read)
      (do ([s (read) (read)]
           [out (list) (if (defining-form? s)
                           (cons (name-getter s) out) out)])
          ((eof-object? s) out)))
    (with-input-from-file file-path do-read)))