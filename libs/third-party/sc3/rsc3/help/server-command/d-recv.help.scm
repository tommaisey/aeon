; /d_recv                           Receive a synth definition file

; bytes - buffer of data.
; bytes - an OSC message to execute upon completion. (optional)

; Loads a file of synth definitions from a buffer in the
; message. Resident definitions with the same names are overwritten.

; Asynchronous. Replies to sender with /done when complete.
