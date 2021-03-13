;; Prevent a chez global lock blocking on the playback thread.
;; This is necessary in an emacs-geiser context, but disables
;; the nice expression editor in the ordinary repl.
(when #f
  (let* ([stdin (transcoded-port (standard-input-port) 
				 (make-transcoder (utf-8-codec)))])
    (current-input-port stdin)
    (console-input-port stdin)))

;; Load libraries, top level functions and state.
(source-directories (list "." "./init"))
(load "libs.scm")

;;------------------------------------------------------------------
(println "------------------------------------------")
(println "[aeon] musical patterns")
(println "------------------------------------------")

;;------------------------------------------------------------------
(define (run-tests)
  (load "libs/tests/context-tests.scm")
  (load "libs/tests/harmony-tests.scm")
  (load "libs/tests/basic-ops-tests.scm")
  (load "libs/tests/time-ops-tests.scm"))

;; Uncomment to run tests on engine init.
(run-tests)

;;------------------------------------------------------------------
(load "sc3.scm")
(load "synthdefs.scm")
(load "buffers.scm")
(load "event-process.scm")
(load "patterns.scm")
(load "recording.scm")
(load "playhead.scm")
(load "comms.scm")

(tag-pdef-callable map)
(tag-pdef-callable filter)

(set-bpm! bpm)
(play)

;; Load user's init files, but not clip files.
(for-each (lambda (p) (when (valid-scheme-path? p) (load p)))
          (child-file-paths "music/"))
