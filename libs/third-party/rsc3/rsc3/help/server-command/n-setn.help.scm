; /n_setn                      Set ranges of a node's control value(s)

; int - node ID
; [
; int or string - a control index or name
; int - number of sequential controls to change (M)
; [
; float - a control value
; ] * M
; ] * N

; Set contiguous ranges of control indices to sets of values. For each
; range, the starting control index is given followed by the number of
; controls to change, followed by the values. If the node is a group,
; then it sets the controls of every node in the group.
