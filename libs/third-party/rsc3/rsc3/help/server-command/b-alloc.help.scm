; /b_alloc                                Allocate buffer space.

; int - buffer number
; int - number of frames
; int - number of channels (optional. default = 1 channel)
; bytes - an OSC message to execute upon completion. (optional)

; Allocates zero filled buffer to number of channels and samples.

; Asynchronous. Replies to sender with /done when complete.
