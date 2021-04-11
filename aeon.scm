#!chezscheme

;;------------------------------------------------------------------
;; Load libraries
(load "runtime/libs.scm")

;;------------------------------------------------------------------
;; Prevent a chez global lock blocking on the playback thread in emacs geiser.
(when (top-level-bound? 'geiser:eval)
  (display "Geiser detected: disabling expression editor")
  (let* ([stdin (transcoded-port (standard-input-port)
				 (make-transcoder (utf-8-codec)))])
    (current-input-port stdin)
    (console-input-port stdin)))

;;------------------------------------------------------------------
;; Load top-level state and functions
(load "runtime/sc3.scm")
(load "runtime/synthdefs.scm")
(load "runtime/buffers.scm")
(load "runtime/event-process.scm")
(load "runtime/patterns.scm")
(load "runtime/recording.scm")
(load "runtime/playhead.scm")

(tag-pdef-callable map)
(tag-pdef-callable filter)

(set-bpm! bpm)
(play)

;;------------------------------------------------------------------
(let ([divider (make-string 60 #\-)])
  (println divider)
  (println "[aeon] musical patterns")
  (println divider))
