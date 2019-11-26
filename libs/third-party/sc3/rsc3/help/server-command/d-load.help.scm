; /d_load                                      Load synth definition

; string - pathname of file. Can be a pattern like "synthdefs/perc-*"
; bytes - an OSC message to execute upon completion. (optional)

; Loads a file of synth definitions. Resident definitions with the same
; names are overwritten.

; Asynchronous. Replies to sender with /done when complete.
