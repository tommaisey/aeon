; /n_free                                          Delete a node.

; [
; int - node ID
; ] * N

; Stops a node abruptly, removes it from its group, and frees its
; memory. A list of node IDs may be specified. Using this method can
; cause a click if the node is not silent at the time it is freed.
