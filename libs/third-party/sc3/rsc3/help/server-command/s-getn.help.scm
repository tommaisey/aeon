; /s_getn                          Get ranges of control value(s)

; int - synth ID
; [
;   int|string - a control index or name
;   int        - number of sequential controls to get (M)
; ] * N

; Get contiguous ranges of controls. Replies to sender with the
; corresponding /n_setn command.
