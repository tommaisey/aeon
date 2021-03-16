(library (file-tools)

  (export valid-scheme-path?
          path-append
          child-file-paths
          definitions-in-file
          file-lines
          os-symbol)

  (import (chezscheme) (utilities))

  ;; Doesn't check it exists, just for an apparently valid path.
  (define (valid-scheme-path? path)
    (string=? "scm" (path-extension path)))

  ;; A safer way to add a file name to a directory
  (define (path-append dir file)
    (let* ([sep (string (directory-separator))]
           [dir (if (string-suffix? sep dir) dir (str+ dir sep))])
      (str+ dir file)))

  ;; Unlike directory-list, returns full path of child files
  (define (child-file-paths dir)
    (map (lambda (f) (path-append dir f))
         (directory-list dir)))

  ;; Expands the home-directory ~ shortcut on Mac.
  (define (expand-path path)
    (if (and (eqv? os-symbol 'macos)
             (eqv? #\~ (string-ref path 0)))
        (error "not implemented yet")
        path))

  ;; Returns a human-readable symbol for the os from Chez's cryptic machine-type.
  (define os-symbol
    (let* ([m-type-list (string->list (symbol->string (machine-type)))]
           [chez-name (list->string (cdr (memp char-numeric? m-type-list)))])
      (cdar (memp (lambda (x) (string=? (car x) chez-name))
                  '(("fb"  . freebsd)
                    ("le"  . linux)
                    ("nb"  . netbsd)
                    ("nt"  . windows)
                    ("ob"  . openbsd)
                    ("osx" . macos)
                    ("qnx" . qnx)
                    ("s2"  . solaris))))))

  ;;-----------------------------------------------------------------------
  ;; Looks for top-level forms in a file matching defining-form? and
  ;; returns the name of the definition using a name-getter fn.
  ;; Warning: currently won't work with definitions in a library or module.
  (define (definitions-in-file file-path defining-form? name-getter)
    (define (do-read)
      (do ([s (read) (read)]
           [out (list) (if (defining-form? s)
                           (cons (name-getter s) out) out)])
          ((eof-object? s) out)))
    (with-input-from-file file-path do-read))

  ;; Simple routine to open a file as a textual (rather than binary) port.
  (define (open-file-as-textual-port file)
    (open-file-input-port file (file-options) 'block (native-transcoder)))
  
  ;; Read a port into a list of lines.
  (define (lines-from-port p)
    (do ([line (get-line p) (get-line p)]
         [lines (list) (cons line lines)])
        ((eof-object? line)
         (begin (close-port p) (reverse lines)))))
  
  ;; Read an entire file into a list of lines.
  ;; Don't do this with huge files!
  (define (file-lines file)
    (lines-from-port (open-file-as-textual-port file)))

  )

(current-directory)
