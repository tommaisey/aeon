;; (pitch in initFreq minFreq maxFreq execFreq maxBinsPerOctave
;;        median ampThreshold peakThreshold downSample clar)

;; Autocorrelation pitch follower

;; This is a better pitch follower than zero-crossing, but more costly
;; of CPU. For most purposes the default settings can be used and only
;; in needs to be supplied. pitch returns two values (via an Array of
;; outputProxys, see the outputProxy help file), a freq which is the
;; pitch estimate and hasFreq, which tells whether a pitch was
;; found. Some vowels are still problematic, for instance a wide open
;; mouth sound somewhere between a low pitched short 'a' sound as in
;; 'sat', and long 'i' sound as in 'fire', contains enough overtone
;; energy to confuse the algorithm.

;; sclang default argument values are: in = 0.0, initFreq = 440.0,
;; minFreq = 60.0, maxFreq = 4000.0, execFreq = 100.0,
;; maxBinsPerOctave = 16, median = 1, ampThreshold = 0.01,
;; peakThreshold = 0.5, downSample = 1.

(define (pitch* in median ampThreshold)
  (pitch in 444.0 60.0 4000.0 100.0 16 median ampThreshold 0.5 1 0))

(let* ((in (mul (sin-osc ar (mouse-x kr 220 660 0 0.1) 0)
		(mouse-y kr 0.05 0.25 0 0.1)))
       (amp (amplitude kr in 0.05 0.05))
       (freq+ (pitch* in 7 0.02))
       (f (fdiv (car (mce-channels freq+)) 2))
       (o (mul (sin-osc ar f 0) amp)))
  (audition (out 0 (mce2 in o))))

(let* ((in (sound-in 0))
       (amp (amplitude kr in 0.05 0.05))
       (freq+ (pitch* in 7 0.02))
       (f (car (mce-channels freq+)))
       (o (mul (sin-osc ar f 0) amp)))
  (audition (out 0 (mce2 in o))))
