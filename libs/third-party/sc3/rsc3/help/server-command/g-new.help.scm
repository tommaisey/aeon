; /g_new				Create a new group

; [
; 	int - new group ID
; 	int - add action (0,1,2, 3 or 4 see below)
; 	int - add target ID
; ] * N
	
; Create a new group and add it to the tree of nodes.

; There are four ways to add the group to the tree as determined by
; the add action argument which is defined as follows (the same as
; for "/s_new"):

; 0 - add the new group to the the head of the group specified by
; the add target ID.

; 1 - add the new group to the the tail of the group specified by
; the add target ID.

; 2 - add the new group just before the node specified by the add
; target ID.

; 3 - add the new group just after the node specified by the add
; target ID.

; 4 - the new node replaces the node specified by the add target
; ID. The target node is freed.

; Multiple groups may be created in one command by adding
; arguments.
