; /b_read             Read sound file data into an existing buffer.

;  int - buffer number
;  string - path name of a sound file.
;  int - starting frame in file (optional. default = 0)
;  int - number of frames to read (optional. default = -1, see below)
;  int - starting frame in buffer (optional. default = 0)
;  int - leave file open (optional. default = 0)
;  bytes - an OSC message to execute upon completion. (optional)
 
; Reads sound file data from the given starting frame in the file and
; writes it to the given starting frame in the buffer. If number of
; frames is less than zero, the entire file is read.  If reading a
; file to be used by DiskIn ugen then you will want to set "leave file
; open" to one, otherwise set it to zero.

; Asynchronous.  Replies to sender with /done when complete.
