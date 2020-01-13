;; Prevent a chez global lock blocking on the playback thread.
(let* ([stdin (transcoded-port (standard-input-port) 
                               (make-transcoder (utf-8-codec)))])
  (current-input-port stdin)
  (console-input-port stdin))

;; Load libraries, top level functions and state.
(source-directories (list "." "./init"))

(load "libs.scm")
(println "Welcome to [unnamed music system]!")

(load "sc3.scm")
(load "synthdefs.scm")
(load "buffers.scm")
(load "event-process.scm")
(load "patterns.scm")
(load "recording.scm")
(load "playhead.scm")

(tag-pdef-callable map)
(tag-pdef-callable filter)

(set-bpm! bpm)
(start)

;; Load user's init files, but not clip files.
(for-each (lambda (p) (when (valid-scheme-path? p) (load p)))
          (child-file-paths "music/"))