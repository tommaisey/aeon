;; Command to begin a recording. User can supply a simple number for
;; the recording length or a specific arc of time. If a simple number,
;; the recording starts at the next whole multiple of that number.
(define-syntax record
  (syntax-rules ()
    ((_ arc/len filepath)
     (cond
      [(not (file-parent-exists? filepath))
       (printfln "Parent directory doesn't exist: ~a" filepath)]
      [(not rendered-point)
       (printfln "Won't record to: ~a. Retry after playhead has begun." filepath)]
      [(not (or (number? arc/len) (arc? arc/len)))
       (printfln "Record: first arg should be an arc or a number: ~a" arc/len)]
      [else
       (let ([bufnum (open-write-buffer (expand-path filepath))]
             [arc (if (number? arc/len)
                      (let ([start (round-up rendered-point arc/len)])
                        (make-arc start (+ start arc/len)))
                      arc/len)])

         (when active-recording
           (lest [synth-id (recording-state-synth active-recording)]
                 (stop-synth synth-id))
           (close-write-buffer (recording-state-bufnum active-recording)))

         (set! active-recording (make-recording-state arc bufnum))
         (printfln "Recording starts in ~~~A measures"
                   (exact (round (- (arc-start arc) rendered-point)))))]))))

;;----------------------------------------------------------------
;; Note: `disk-out` fails if num input channels doesn't match the
;; buffer's channels. Currently we always assume stereo.
(send-synth sc3 "recorder2"
  (letc ([:inbus 0] [:outbuf -1])
    (disk-out :outbuf (stereo-in :inbus))))

(define-immutable-record recording-state
  [arc (make-arc 0 1)]
  [bufnum -1]
  [synth #f])

;; A global recording-state (optional).
(define active-recording #f)

;; Called regularly from the playback thread. If the user has requested
;; a recording, it starts and ends a diskout synth at the right time.
(define (update-recording beat-now render-arc t)
  (when active-recording
    (let* ([synth-id (recording-state-synth active-recording)]
           [rec-arc (recording-state-arc active-recording)]
           [start (arc-start rec-arc)]
           [end (arc-end rec-arc)])
      (cond
        ((and (not synth-id) (within-arc? render-arc start))
         (start-recording (time-at-beat start beat-now t)))

        ((and synth-id (within-arc? render-arc end))
         (cancel-recording (time-at-beat end beat-now t)))))))

(define* (start-recording [/opt (t (sc/utc))])
  (when active-recording
    (let* ([bufnum (recording-state-bufnum active-recording)]
           [event (make-event 0 :inbus 0 :outbuf bufnum)]
           [args (event-symbols->strings event)]
           [synth-id 55378008]) ; only one at a time for now
      (play-when "recorder2" t args recording-group sc/add-to-head synth-id)
      (set! active-recording (recording-state-with-synth active-recording synth-id))
      (println "Recording started!"))))

(define* (cancel-recording [/opt (t (sc/utc))])
  (when active-recording
    (let* ([synth-id (recording-state-synth active-recording)]
           [bufnum (recording-state-bufnum active-recording)])
      (stop-synth synth-id t)
      (close-write-buffer bufnum (+ t 0.01))
      (set! active-recording #f)
      (println "Recording ended!"))))
