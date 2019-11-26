; /c_setn                                   Set ranges of bus value(s)

; [
; int - starting bus index
; int - number of sequential buses to change (M)
; [
; float - a control value
; ] * M
; ] * N

; Set contiguous ranges of buses to sets of values. For each range, the
; starting bus index is given followed by the number of channels to
; change, followed by the values.
