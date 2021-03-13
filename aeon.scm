;; Prevent a chez global lock blocking on the playback thread.
;; This is necessary in an emacs-geiser context, but disables
;; the nice expression editor in the ordinary repl.
(when #f
  (let* ([stdin (transcoded-port (standard-input-port) 
				 (make-transcoder (utf-8-codec)))])
    (current-input-port stdin)
    (console-input-port stdin)))

;; Load libraries
(source-directories (list "." "./runtime"))
(load "libs.scm")

;;------------------------------------------------------------------
(println "------------------------------------------")
(println "[aeon] musical patterns")
(println "------------------------------------------")

;;------------------------------------------------------------------
;; Load top-level state and functions
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
