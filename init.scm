;; -*- geiser-scheme-implementation: chez-*-
(load "libs.scm")

;; Adds custom priting of contexts.
(record-writer (type-descriptor context) context-print)

(load "init-sc3.scm")
(load "init-event-process.scm")
(load "init-playback.scm")