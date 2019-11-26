; /sync                      Notify when async commands have completed.

; int - a unique number identifying this command.

; Replies with a /synced message when all asynchronous commands
; received before this one have completed. The reply will contain the
; sent unique ID.

; Asynchronous. Replies to sender with /synced, ID when complete.
