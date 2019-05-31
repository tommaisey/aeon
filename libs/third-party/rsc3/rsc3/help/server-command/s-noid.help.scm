; /s_noid               Auto-reassign synth's ID to a reserved value

; [
; int - synth ID
; ] * N

; This command is used when the client no longer needs to communicate
; with the synth and wants to have the freedom to reuse the ID. The
; server will reassign this synth to a reserved negative number. This
; command is purely for bookkeeping convenience of the client. No
; notification is sent when this occurs.
