; /n_mapn                       Map a node's controls to read from buses

; int - node ID
; [
; int or string - a control index or name
; int - control bus index
; int - number of controls to map
; ] * N

; Takes a list of triplets of control names or indices, bus indices, and
; number of controls to map and causes those controls to be mapped
; sequentially to buses. If the node is a group, then it maps the
; controls of every node in the group. If the control bus index is -1
; then any current mapping is undone and control reverts to normal.

; See also: /n_map
