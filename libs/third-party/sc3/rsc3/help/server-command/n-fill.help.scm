; /n_fill                    Fill ranges of a node's control value(s)

; int - node ID
; [
; int or string - a control index or name
; int - number of values to fill (M)
; float - value
; ] * N

; Set contiguous ranges of control indices to single values. For each
; range, the starting control index is given followed by the number of
; controls to change, followed by the value to fill. If the node is a
; group, then it sets the controls of every node in the group.
