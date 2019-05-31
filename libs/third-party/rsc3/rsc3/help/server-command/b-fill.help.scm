; /b_fill                              Fill ranges of sample value(s)

; int - buffer number
; [
;  int - sample starting index
;  int - number of samples to fill (M)
;  float - value
; ] * N
	
; Set contiguous ranges of sample indices to single values. For each
; range, the starting sample index is given followed by the number of
; samples to change, followed by the value to fill. This is only meant
; for setting a few samples, not whole buffers or large sections.
