;; (buf-channels rate bufnum)

;; Current number of channels of buffer.  Using at .ir is not the
;; safest choice. Since a buffer can be reallocated at any time, using
;; ir will not track the changes.
