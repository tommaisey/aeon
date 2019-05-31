;; (compander input control thresh slopeBelow slopeAbove clampTime relaxTime)

;; Compressor, expander, limiter, gate, ducker.  General purpose
;; dynamics processor.

;; input: The signal to be compressed / expanded / gated.

;; control: The signal whose amplitude determines the gain applied to
;;          the input signal. Often the same as in (for standard
;;          gating or compression) but should be different for
;;          ducking.

;; thresh: Control signal amplitude threshold, which determines the
;;         break point between slopeBelow and slopeAbove. Usually
;;         0..1. The control signal amplitude is calculated using RMS.

;; slopeBelow: slope of the amplitude curve below the threshold. If
;;             this slope > 1.0, the amplitude will drop off more
;;             quickly the softer the control signal gets when the
;;             control signal is close to 0 amplitude, the output
;;             should be exactly zero -- hence, noise gating. Values <
;;             1.0 are possible, but it means that a very low-level
;;             control signal will cause the input signal to be
;;             amplified, which would raise the noise floor.

;; slopeAbove: Same thing, but above the threshold. Values < 1.0
;;             achieve compression (louder signals are attenuated) >
;;             1.0, you get expansion (louder signals are made even
;;             louder). For 3:1 compression, you would use a value of
;;             1/3 here.

;; clampTime: The amount of time it takes for the amplitude adjustment
;;            to kick in fully. This is usually pretty small, not much
;;            more than 10 milliseconds (the default value).

;; relaxTime: The amount of time for the amplitude adjustment to be
;;            released. Usually a bit longer than clampTime if both
;;            times are too short, you can get some (possibly
;;            unwanted) artifacts.

;; Example signal to process.

(define z
  (mul (decay2 (mul (impulse ar 8 0) (mul (lf-saw kr 0.3 0) 0.3)) 0.001 0.3)
       (mix (pulse ar (mce2 80 81) 0.3))))

(audition (out 0 z))

;; Noise gate

(let ((x (mouse-x kr 0.01 1 0 0.1)))
  (audition (out 0 (mce2 z (compander z z x 10 1 0.01 0.01)))))

;; Compressor

(let ((x (mouse-x kr 0.01 1 0 0.1)))
  (audition (out 0 (mce2 z (compander z z x 1 0.5 0.01 0.01)))))

;; limiter

(let ((x (mouse-x kr 0.01 1 0 0.1)))
  (audition (out 0 (mce2 z (compander z z x 1 0.1 0.01 0.01)))))

;; Sustainer

(let ((x (mouse-x kr 0.01 1 0 0.1)))
  (audition (out 0 (mce2 z (compander z z x 0.1 1.0 0.01 0.01)))))
