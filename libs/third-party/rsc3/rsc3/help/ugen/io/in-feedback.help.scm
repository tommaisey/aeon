;; (in-feedback num-channels bus)

;; Read signal from a bus without erasing it, audio rate.

;; The output (out) ugens overwrite data on the bus, giving this bus a
;; new timestamp so that any input (in) ugen can check if the data was
;; written within the current cycle. The next cycle this data is still
;; there, but in case of audio one normally doesn't want an in ugen to
;; read it again, as it might cause feedback.

;; This is the reason why in ar checks the timestamp and ignores
;; everything that was not written within this cycle. This means that
;; nodes can only read data from a bus that was written by a
;; preceeding node when using the in ar ugen which overwrites the old
;; data. This is good for audio, but for control data it is more
;; convenient to be able to read a bus from any place in the node
;; order.

;; This is why in kr behaves differently and reads also data with a
;; timestamp that is one cycle old. Now in some cases we want to be
;; able to read audio from a bus independant of the current node
;; order, which is the use of inFeedback.  The delay introduced by
;; this is at a maximum one block size, which equals about 0.0014 sec
;; at the default block size and sample rate.

;; Audio feedback modulation.

(let ((f (mul-add (in-feedback 1 0) 1300 300)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.4))))

;; Evaluate these in either order and hear both tones.

(let ((b (add num-input-buses num-output-buses)))
  (audition (out 0 (in-feedback 1 b))))

(let ((b (add num-input-buses num-output-buses)))
  (audition (mrg2 (out b (mul (sin-osc ar 440 0) 0.1))
		  (out 0 (mul (sin-osc ar 660 0) 0.1)))))

;; Doubters consult this.

(let ((b (add num-input-buses num-output-buses)))
  (audition (out 0 (in 1 ar b))))

;; Resonator, see localout for variant.

(let* ((b (add num-input-buses num-output-buses))
       (p (in-feedback 1 b))
       (i (impulse ar 1 0))
       (d (delay-c (add i (mul p 0.995)) 
		   1 
		   (sub (recip 440) (recip control-rate)))))
  (audition (mrg2 (offset-out b d) (offset-out 0 p))))

;; Compare with oscillator.

(audition (out 1 (mul (sin-osc ar 440 0) 0.2)))
