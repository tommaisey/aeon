; /n_map                        Map a node's controls to read from a bus

; int - node ID
; [
; int or string - a control index or name
; int - control bus index
; ] * N

; Takes a list of pairs of control names or indices and bus indices and
; causes those controls to be read continuously from a global control
; bus instead of responding to n_set, n_setn and n_fill commands. If the
; node is a group, then it maps the controls of every node in the
; group. If the control bus index is -1 then any current mapping is
; undone and control reverts to normal.

