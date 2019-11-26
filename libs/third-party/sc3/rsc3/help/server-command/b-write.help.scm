; /b_write					Write sound file data.

;  int - buffer number
;  string - path name of a sound file.
;  string - header format.
;  string - sample format.
;  int - number of frames to write (optional. default = -1, see below)
;  int - starting frame in buffer (optional. default = 0)
;  int - leave file open (optional. default = 0)
;  bytes - an OSC message to execute upon completion. (optional)

; Write a buffer as a sound file.
; Header format is one of:
;  "aiff", "next", "wav", "ircam"", "raw"
; Sample format is one of:
;  "int8", "int16", "int24", "int32", "float", "double", "mulaw", "alaw"

; Not all combinations of header format and sample format are
; possible.  If number of frames is less than zero, all samples from
; the starting frame to the end of the buffer are written.  If opening
; a file to be used by DiskOut ugen then you will want to set "leave
; file open" to one, otherwise set it to zero. If "leave file open" is
; set to one then the file is created, but no frames are written until
; the DiskOut ugen does so.

; Asynchronous. Replies to sender with /done when complete.

