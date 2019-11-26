; /b_allocRead           Allocate buffer space and read a sound file.

; int - buffer number
; string - path name of a sound file.
; int - starting frame in file (optional. default = 0)
; int - number of frames to read (optional. default = 0, see below)
; bytes - an OSC message to execute upon completion. (optional)

; Allocates buffer to number of channels of file and number of samples
; requested, or fewer if sound file is smaller than requested. Reads
; sound file data from the given starting frame in the file. If the
; number of frames argument is less than or equal to zero, the entire
; file is read.

; Asynchronous. Replies to sender with /done when complete.

