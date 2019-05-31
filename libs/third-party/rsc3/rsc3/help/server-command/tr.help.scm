; /tr				                   A trigger message

; int - node ID
; int - trigger ID
; float - trigger value

; This command is the mechanism that synths can use to trigger events
; in clients.

; The node ID is the node that is sending the trigger. The trigger ID
; and value are determined by inputs to the SendTrig unit generator
; which is the originator of this message.
