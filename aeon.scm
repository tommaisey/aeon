#!chezscheme

;;------------------------------------------------------------------
;; Load libraries
(source-directories (list "." "./runtime"))
(load "libs.scm")

;;------------------------------------------------------------------
;; Prevent a chez global lock blocking on the playback thread in emacs geiser.
(when (top-level-bound? 'geiser:eval)
  (display "Geiser detected: disabling expression editor")
  (let* ([stdin (transcoded-port (standard-input-port)
				 (make-transcoder (utf-8-codec)))])
    (current-input-port stdin)
    (console-input-port stdin)))

;;------------------------------------------------------------------
(let ([divider (fold-left str+ "" (repeat 60 "-"))])
  (println divider)
  (println "[aeon] musical patterns")
  (println divider))

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
