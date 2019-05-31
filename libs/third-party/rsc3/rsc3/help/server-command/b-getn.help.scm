; /b_getn                               Get ranges of sample value(s)

; int - buffer number
; [
;  int - starting sample index
;  int - number of sequential samples to get (M)
; ] * N

; Get contiguous ranges of samples. Replies to sender with the
; corresponding /b_setn command. This is only meant for getting a few
; samples, not whole buffers or large sections.
