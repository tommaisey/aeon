;; Stop a mutex in the innards of Chez from blocking in Emacs
;; REPLs, which prevents our background thread from running.
(define (fix-emacs-repl)
  (let ([stdin (transcoded-port (standard-input-port)
                                (make-transcoder (utf-8-codec)))])
    (current-input-port stdin)
    (console-input-port stdin)))
(fix-emacs-repl)

;; Load libraries, top level functions and state.
(source-directories (list "." "./init"))
(load "libs.scm")
(load "sc3.scm")
(load "synthdefs.scm")
(load "event-process.scm")
(load "patterns.scm")
(load "playhead.scm")

;; Load music files intended to be shared, but not clip files.
(for-each (lambda (p) (when (valid-scheme-path? p) (load p)))
          (child-file-paths "music/"))

(set-bpm! bpm)
(start)

;; Custom priting of contexts.
(record-writer (type-descriptor context) context-print)