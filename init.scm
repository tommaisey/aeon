;; -*- geiser-scheme-implementation: chez-*-
(load "libs.scm")

;;-----------------------------------------------------------------
;; Some SuperCollider setup: a default server, synthdef & event sender.
;; TODO: blocks if SuperCollider isn't running!! 
(define s (udp:open "127.0.0.1" 57110))
(send s (g-new1 1 add-to-tail 0))

(define (play-when name t . arg-pairs)
  (let ([args (list (s-new0 name -1 add-to-tail 1)
		    (n-set -1 arg-pairs))])
    (send s (bundle t args))))

(define (play-now name . arg-pairs)
  (play-when name (utc) arg-pairs))

(send-synth
 s "sine-grain"
 (letc ([freq 440] [attack 0.01] [sustain 1])
   (let* ([osc (sin-osc ar freq 0)]
	  [env (env-perc attack sustain 1 (list -4 -4))]
	  [env (env-gen kr 1 0.2 0 1 remove-synth env)])
     (out 0 (mul env osc)))))

(send-synth
 s "sampler-mono"
 (letc ([sample 0] [attack 0.01] [sustain 1] [pan 0.5] [speed 1] [release 0.25] [amp 0.3])
   (let* ([rte (mul speed (buf-rate-scale kr sample))]
	  [osc (play-buf 1 ar sample rte 1 0 no-loop remove-synth)]
	  [env (env-linen attack sustain release 1 '(-4 -4 -4))]
	  [env (env-gen kr 1 amp 0 1 remove-synth env)])
     (out 0 (pan2 (mul env osc) (add (mul pan 2) -1) 1)))))

(define (add-sample path bufnum)
  (async s (b-alloc-read bufnum path 0 0)))

(define (check-sample bufnum)
  (async s (b-query1 bufnum)))

(define (sine-perc freq length)
  (play-now "sine-grain"
	    (cons "freq" freq)
	    (cons "length" length)))

;;-----------------------------------------------------------------
;; Plays a note at the right time in the future.
(define (play-note note current-beat)
  (define (entry-convert pair)
    (cons (symbol->string (car pair)) (cdr pair)))
  (let* ([inst (note-get note 'inst "sine-grain")]
	 [beat (note-beat note)]
	 [until (secs-until beat current-beat bpm)]
	 [t (+ (utc) until playback-latency)]) ;; TODO: adjust latency based on frame jitter
    (apply play-when inst t (map entry-convert (note-clean note)))))

;; Our test pattern for the moment. Redefine for fun and profit!
(define *1 (cycle (event [1 3])))

;; Called each chunk of time by the playback thread.  
(define (process-chunk)
  (let* ([t playback-time]
	 [nxt-t (+ t playback-chunk)]
	 [c (make-empty-context t nxt-t)])
    (for-each (lambda (n) (play-note n t)) (context-notes-next (*1 c)))
    (set! playback-time nxt-t)))

;;-----------------------------------------------------------------
;; A thread that wakes up every playback-chunk beats to call (process-chunk)
;; This essentially controls playback for now.
(define bpm 120)
(define playback-thread #f)
(define playback-chunk 1/4) ; 1 quarter note for now
(define playback-thread-semaphore (make-semaphore))
(define playback-time 0)
(define playback-latency 0.2)

;; Only creates new thread if one isn't already in playback-thread.
(define (start-thread sem)
  (when (not playback-thread)
    (set! playback-thread
      (start-suspendable-thread
       process-chunk (* playback-chunk (bpm->spm bpm)) sem))))

(define (start)
  (start-thread playback-thread-semaphore)
  (stop-waiting playback-thread-semaphore))

(define (pause)
  (start-waiting playback-thread-semaphore))

(define (stop)
  (pause)
  (set! playback-time 0))

;;------------------------------------------------------------------
;; Set up some inital drum samples
(begin
  (define drum-dir
  "/Users/tommaisey/Dropbox/Music Production/Samples & Patches/Drum Samples/")
  (define (drum-path file)
    (string-append drum-dir file))
  (define (tape-drum file)
    (string-append drum-dir "SM43 - Vinyl and Tape Drum Hits/" file))
  (define (mbase-kik file)
    (string-append drum-dir
		   "WA_Drum_Machine_Collection/WA_Drum Machines_01/"
		   "WA_Airbase Drums/kick drums/mbase 11/" file))
 
  (begin (define bd 1) (add-sample (mbase-kik "waad_mbase_kick_120.wav") 1))
  (begin (define sn 2) (add-sample (tape-drum "tape/snares/machine/SM43_vth_tpe_snr_mch_slammin.wav") 2))
  (begin (define hh 3) (add-sample (tape-drum "tape/hi-hats/closed/SM43_vth_tpe_hh_clsd_hitin.wav") 3))
  (begin (define oh 4) (add-sample (tape-drum "tape/hi-hats/open/SM43_vth_tpe_hh_op_itsgood.wav") 4))
  (begin (define xt 5) (add-sample (drum-path "Cross Sticks/82280__kevoy__acoustic-side-stick.wav") 5)))
