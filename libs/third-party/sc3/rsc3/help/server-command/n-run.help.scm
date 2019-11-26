; /n_run                                          Turn node on or off

; [
; int - node ID
; int - run flag
; ] * N

; If the run flag set to zero then the node will not be executed.  If
; the run flag is set back to one, then it will be executed.  Using
; this method to start and stop nodes can cause a click if the node is
; not silent at the time run flag is toggled.
