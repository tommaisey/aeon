;; (pv-jensen-andersen buffer propsc prophfe prophfc propsf threshold waittime)

#|

fft feature detector for onset detection based on work described in
Jensen,K. & Andersen, T. H. (2003). Real-time Beat Estimation Using
Feature Extraction. in Proceedings of the Computer Music Modeling and
Retrieval Symposium, Lecture Notes in Computer Science. springer
Verlag.

First order derivatives of the features are taken. Threshold may
need to be set low to pick up on changes.

buffer    - fft buffer to read from.
propsc    - Proportion of spectral centroid feature.
prophfe   - Proportion of high frequency energy feature.
prophfc   - Proportion of high frequency content feature.
propsf    - Proportion of spectral flux feature.
threshold - Threshold level for allowing a detection
waittime  - If triggered, minimum wait until a further frame can
            cause another spot (useful to stop multiple detects on
            heavy signals)

Default values in sclang are: propsc=0.25, prophfe=0.25,
prophfc=0.25, propsf=0.25, threshold=1.0, waittime=0.04.

|#

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 0 2048 1))))

(let* ((source (sound-in 0))
       (detect (pv-jensen-andersen (fft* 0 source)
				  0.25 0.25 0.25 0.25
				  (mouse-x kr 0.01 1.0 1 0.1)
				  0.04)))
  (audition
   (out 0 (mul (sin-osc ar (mce2 440 445) 0)
	       (decay (mul 0.1 detect) 0.1)))))
