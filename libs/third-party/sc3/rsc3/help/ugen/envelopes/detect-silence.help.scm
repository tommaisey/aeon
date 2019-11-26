;; (detect-silence in amp time doneAction)

;; If the signal at `in' falls below `amp' for `time' seconds then
;; `doneAction' is raised.

(let ((s (mul (sin-osc ar 440 0) (mouse-y kr 0 0.4 0 0.1))))
  (audition (mrg2 (detect-silence s 0.1 0.2 remove-synth)
		  (out 0 s))))

(with-sc3 display-server-status)
