; /b_setn                               Set ranges of sample value(s)

; int - buffer number
; [
; int - sample starting index
; int - number of sequential samples to change (M)
; [
; float - a sample value
; ] * M
; ] * N

; Set contiguous ranges of sample indices to sets of values. For each
; range, the starting sample index is given followed by the number of
; samples to change, followed by the values.

