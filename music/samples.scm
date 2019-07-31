
(define drum-dir
  "/Users/tommaisey/Dropbox/Music Production/Samples & Patches/Drum Samples/")
(define (tape-drum . fs)
  (apply string-append drum-dir "SM43 - Vinyl and Tape Drum Hits/" fs))
(define (mbase-kik . fs)
  (apply string-append drum-dir
         "WA_Drum_Machine_Collection/WA_Drum Machines_01/"
         "WA_Airbase Drums/kick drums/mbase 11/" fs))

(samples-dir bd (mbase-kik))
(samples-dir sn (tape-drum "tape/snares/machine/"))
(samples-dir hh (tape-drum "tape/hi-hats/closed/"))
(samples-dir oh (tape-drum "tape/hi-hats/open/"))
(samples-dir xt (path-append drum-dir "Cross Sticks/"))

(samples-dir cy
  (string-append
   "/Users/tommaisey/Dropbox/Music Production/"
   "Samples & Patches/CC Samples/Real Instruments/Ghana Bells"))

(samples-dir wind
  (string-append
   "/Users/tommaisey/Dropbox/Music Production/Samples & Patches/"
   "CC Samples/Real Instruments/Woodwind"))
